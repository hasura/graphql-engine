package hasuradb

import (
	"crypto/tls"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	nurl "net/url"
	"path"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/statestore"

	"github.com/hasura/graphql-engine/cli/internal/statestore/settings"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/pkg/errors"

	"github.com/mitchellh/mapstructure"

	yaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/parnurzeal/gorequest"
	log "github.com/sirupsen/logrus"
)

func init() {
	db := HasuraDB{}
	database.Register("hasuradb", &db)
}

const (
	DefaultDatabaseName = "default"
)

var (
	ErrNilConfig      = fmt.Errorf("no config")
	ErrNoDatabaseName = fmt.Errorf("no database name")
	ErrNoSchema       = fmt.Errorf("no schema")
	ErrDatabaseDirty  = fmt.Errorf("database is dirty")

	queryTypes = []string{
		"select", "insert", "select", "update", "delete", "count", "run_sql", "bulk",
		"mssql_select", "mssql_insert", "msssql_select", "mssql_update", "mssql_delete", "mssql_count", "mssql_run_sql",
	}
	queryTypesMap = func() map[string]bool {
		var m = map[string]bool{}
		for _, v := range queryTypes {
			m[v] = true
		}
		return m
	}()
)

type Config struct {
	MigrationsTable                string
	SettingsTable                  string
	queryURL                       *nurl.URL
	metadataURL                    *nurl.URL
	graphqlURL                     *nurl.URL
	pgDumpURL                      *nurl.URL
	Headers                        map[string]string
	isCMD                          bool
	enableCheckMetadataConsistency bool
	Req                            *gorequest.SuperAgent
}

type HasuraDB struct {
	config         *Config
	settings       []settings.Setting
	migrations     *database.Migrations
	migrationQuery HasuraInterfaceBulk
	jsonPath       map[string]string
	isLocked       bool
	logger         *log.Logger
	hasuraOpts     *database.HasuraOpts

	metadataops          hasura.CommonMetadataOperations
	v2metadataops        hasura.V2CommonMetadataOperations
	pgSourceOps          hasura.PGSourceOps
	mssqlSourceOps       hasura.MSSQLSourceOps
	genericQueryRequest  hasura.GenericSend
	hasuraClient         *hasura.Client
	migrationsStateStore statestore.MigrationsStateStore
	settingsStateStore   statestore.SettingsStateStore
}

func WithInstance(config *Config, logger *log.Logger, hasuraOpts *database.HasuraOpts) (database.Driver, error) {
	if config == nil {
		logger.Debug(ErrNilConfig)
		return nil, ErrNilConfig
	}

	hx := &HasuraDB{
		config:     config,
		migrations: database.NewMigrations(),
		settings:   settings.Settings,
		logger:     logger,
		hasuraOpts: hasuraOpts,

		metadataops:         hasuraOpts.MetadataOps,
		v2metadataops:       hasuraOpts.V2MetadataOps,
		pgSourceOps:         hasuraOpts.PGSourceOps,
		mssqlSourceOps:      hasuraOpts.MSSQLSourceOps,
		genericQueryRequest: hasuraOpts.GenericQueryRequest,

		hasuraClient: hasuraOpts.Client,

		migrationsStateStore: hasuraOpts.MigrationsStateStore,
		settingsStateStore:   hasuraOpts.SettingsStateStore,
	}

	if err := hx.migrationsStateStore.PrepareMigrationsStateStore(); err != nil {
		logger.Debug(err)
		return nil, err
	}
	if err := hx.settingsStateStore.PrepareSettingsDriver(); err != nil {
		logger.Debug(err)
		return nil, err
	}

	return hx, nil
}

func (h *HasuraDB) Open(url string, isCMD bool, tlsConfig *tls.Config, logger *log.Logger, hasuraOpts *database.HasuraOpts) (database.Driver, error) {
	if logger == nil {
		logger = log.New()
	}
	hurl, err := nurl.Parse(url)
	if err != nil {
		logger.Debug(err)
		return nil, err
	}
	// Use sslMode query param to set Scheme
	var scheme string
	params := hurl.Query()
	sslMode := params.Get("sslmode")
	if sslMode == "enable" {
		scheme = "https"
	} else {
		scheme = "http"
	}

	headers := make(map[string]string)
	if queryHeaders, ok := params["headers"]; ok {
		for _, header := range queryHeaders {
			headerValue := strings.SplitN(header, ":", 2)
			if len(headerValue) == 2 && headerValue[1] != "" {
				headers[headerValue[0]] = headerValue[1]
			}
		}
	}

	req := gorequest.New()
	if tlsConfig != nil {
		req.TLSClientConfig(tlsConfig)
	}

	config := &Config{
		queryURL: &nurl.URL{
			Scheme: scheme,
			Host:   hurl.Host,
			Path:   path.Join(hurl.Path, params.Get("query")),
		},
		metadataURL: &nurl.URL{
			Scheme: scheme,
			Host:   hurl.Host,
			Path:   path.Join(hurl.Path, params.Get("metadata")),
		},
		graphqlURL: &nurl.URL{
			Scheme: scheme,
			Host:   hurl.Host,
			Path:   path.Join(hurl.Path, params.Get("graphql")),
		},
		pgDumpURL: &nurl.URL{
			Scheme: scheme,
			Host:   hurl.Host,
			Path:   path.Join(hurl.Path, params.Get("pg_dump")),
		},
		isCMD:   isCMD,
		Headers: headers,
		Req:     req,
	}
	hx, err := WithInstance(config, logger, hasuraOpts)
	if err != nil {
		logger.Debug(err)
		return nil, err
	}
	return hx, nil
}

func (h *HasuraDB) Close() error {
	// nothing do to here
	return nil
}

func (h *HasuraDB) Scan() error {
	h.migrations = database.NewMigrations()
	return h.getVersions()
}

func (h *HasuraDB) Lock() error {
	if h.isLocked {
		return database.ErrLocked
	}

	h.migrationQuery = HasuraInterfaceBulk{
		Type: "bulk",
		Args: make([]interface{}, 0),
	}
	h.jsonPath = make(map[string]string)
	h.isLocked = true
	return nil
}

func (h *HasuraDB) UnLock() error {
	if !h.isLocked {
		return nil
	}

	defer func() {
		h.isLocked = false
	}()
	return nil
}

func (h *HasuraDB) Run(migration io.Reader, fileType, fileName string) error {
	migr, err := ioutil.ReadAll(migration)
	if err != nil {
		return err
	}
	body := string(migr[:])
	switch fileType {
	case "sql":
		if body == "" {
			break
		}
		sqlInput := hasura.PGRunSQLInput{
			SQL:    string(body),
			Source: h.hasuraOpts.SourceName,
		}
		if h.config.enableCheckMetadataConsistency {
			sqlInput.CheckMetadataConsistency = func() *bool { b := false; return &b }()
		}
		switch h.hasuraOpts.SourceKind {
		case hasura.SourceKindPG:
			_, err := h.pgSourceOps.PGRunSQL(sqlInput)
			if err != nil {
				return err
			}
		case hasura.SourceKindMSSQL:
			_, err := h.mssqlSourceOps.MSSQLRunSQL(hasura.MSSQLRunSQLInput(sqlInput))
			if err != nil {
				return err
			}
		default:
			return fmt.Errorf("unsupported source kind, source name: %v kind: %v", h.hasuraOpts.SourceName, h.hasuraOpts.SourceKind)
		}
	case "meta":
		var metadataRequests []interface{}
		err := yaml.Unmarshal(migr, &metadataRequests)
		if err != nil {
			h.migrationQuery.ResetArgs()
			return err
		}
		return sendMetadataMigrations(h, metadataRequests)
	}
	return nil
}

func sendMetadataMigrations(hasuradb *HasuraDB, requests []interface{}) error {
	var metadataRequests []interface{}
	var queryRequests []interface{}
	isQueryRequest := func(req interface{}) bool {
		var isIt = false
		type request struct {
			Type string        `mapstructure:"type"`
			Args []interface{} `mapstructure:"args"`
		}
		var r = new(request)
		if err := mapstructure.Decode(req, r); err != nil {
			if _, ok := queryTypesMap[r.Type]; ok {
				isIt = true
			} else {
				return isIt
			}
		}
		return isIt
	}
	for _, v := range requests {
		type bulkQuery struct {
			Type string        `mapstructure:"type"`
			Args []interface{} `mapstructure:"args"`
		}
		var bulk = new(bulkQuery)
		if _ = mapstructure.Decode(v, bulk); bulk.Type == "bulk" {
			queryBulk := HasuraInterfaceBulk{
				Type: "bulk",
				Args: []interface{}{},
			}
			metadataBulk := HasuraInterfaceBulk{
				Type: "bulk",
				Args: []interface{}{},
			}
			for _, bulkRequest := range bulk.Args {
				if ok := isQueryRequest(v); ok {
					queryBulk.Args = append(queryBulk.Args, bulkRequest)
				} else {
					metadataBulk.Args = append(metadataBulk.Args, bulkRequest)
				}
			}
			metadataRequests = append(metadataRequests, metadataBulk)
			queryRequests = append(queryRequests, queryBulk)
		} else {
			if ok := isQueryRequest(v); ok {
				queryRequests = append(queryRequests, v)
			} else {
				metadataRequests = append(metadataRequests, v)
			}
		}
	}
	if len(queryRequests) > 0 {
		queryBulk := HasuraInterfaceBulk{
			Type: "bulk",
			Args: queryRequests,
		}
		resp, body, err := hasuradb.genericQueryRequest(queryBulk)
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			b, err := ioutil.ReadAll(body)
			if err != nil {
				return err
			}
			return errors.New(string(b))
		}

	}
	if len(metadataRequests) > 0 {
		metadataBulk := HasuraInterfaceBulk{
			Type: "bulk",
			Args: metadataRequests,
		}
		resp, body, err := hasuradb.metadataops.SendCommonMetadataOperation(metadataBulk)
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			b, err := ioutil.ReadAll(body)
			if err != nil {
				return err
			}
			return errors.New(string(b))
		}
	}
	return nil
}

func (h *HasuraDB) ResetQuery() {
	h.migrationQuery.ResetArgs()
}

func (h *HasuraDB) InsertVersion(version int64) error {
	return h.migrationsStateStore.InsertVersion(h.hasuraOpts.SourceName, version)
}

func (h *HasuraDB) SetVersion(version int64, dirty bool) error {
	return h.migrationsStateStore.SetVersion(h.hasuraOpts.SourceName, version, dirty)
}

func (h *HasuraDB) RemoveVersion(version int64) error {
	return h.migrationsStateStore.RemoveVersion(h.hasuraOpts.SourceName, version)
}

func (h *HasuraDB) getVersions() (err error) {

	v, err := h.migrationsStateStore.GetVersions(h.hasuraOpts.SourceName)
	if err != nil {
		return err
	}
	for version, dirty := range v {
		h.migrations.Append(database.MigrationVersion{Version: version, Dirty: dirty})
	}
	return nil
}

func (h *HasuraDB) Version() (version int64, dirty bool, err error) {
	tmpVersion, ok := h.migrations.Last()
	if !ok {
		return database.NilVersion, false, nil
	}

	return int64(tmpVersion.Version), tmpVersion.Dirty, nil
}

func (h *HasuraDB) Drop() error {
	return nil
}

func (h *HasuraDB) sendSchemaDumpQuery(m interface{}) (resp *http.Response, body []byte, err error) {
	request := h.config.Req.Clone()

	request = request.Post(h.config.pgDumpURL.String()).Send(m)

	for headerName, headerValue := range h.config.Headers {
		request.Set(headerName, headerValue)
	}

	resp, body, errs := request.EndBytes()

	if len(errs) == 0 {
		err = nil
	} else {
		err = errs[0]
	}

	return resp, body, err
}

func (h *HasuraDB) First() (migrationVersion *database.MigrationVersion, ok bool) {
	return h.migrations.First()
}

func (h *HasuraDB) Last() (*database.MigrationVersion, bool) {
	return h.migrations.Last()
}

func (h *HasuraDB) Prev(version uint64) (prevVersion *database.MigrationVersion, ok bool) {
	return h.migrations.Prev(version)
}

func (h *HasuraDB) Next(version uint64) (migrationVersion *database.MigrationVersion, ok bool) {
	return h.migrations.Next(version)
}

func (h *HasuraDB) Read(version uint64) (ok bool) {
	return h.migrations.Read(version)
}

func (h *HasuraDB) Query(data interface{}) error {
	resp, body, err := h.metadataops.SendCommonMetadataOperation(data)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		b, err := ioutil.ReadAll(body)
		if err != nil {
			return err
		}
		return errors.New(string(b))
	}
	return nil
}
