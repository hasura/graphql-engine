package hasuradb

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	nurl "net/url"
	"path"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore/settings"
	"github.com/hasura/graphql-engine/cli/v2/migrate/database"

	"github.com/mitchellh/mapstructure"
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
		"select", "insert", "update", "delete", "count", "run_sql", "bulk",
		"mssql_select", "mssql_insert", "mssql_update", "mssql_delete", "mssql_count", "mssql_run_sql",
		"citus_select", "citus_insert", "citus_update", "citus_delete", "citus_count", "citus_run_sql",
		"bigquery_select", "bigquery_insert", "bigquery_update", "bigquery_delete", "bigquery_count", "bigquery_run_sql",
		"cockroach_run_sql",
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
	pgDumpClient         hasura.PGDump
	pgSourceOps          hasura.PGSourceOps
	mssqlSourceOps       hasura.MSSQLSourceOps
	citusSourceOps       hasura.CitusSourceOps
	bigquerySourceOps    hasura.BigQuerySourceOps
	cockroachSourceOps   hasura.CockroachSourceOps
	genericQueryRequest  hasura.GenericSend
	hasuraClient         *hasura.Client
	migrationsStateStore statestore.MigrationsStateStore
	settingsStateStore   statestore.SettingsStateStore
}

func WithInstance(config *Config, logger *log.Logger, hasuraOpts *database.HasuraOpts) (database.Driver, error) {
	var op errors.Op = "hasuradb.WithInstance"
	if config == nil {
		logger.Debug(ErrNilConfig)
		return nil, errors.E(op, ErrNilConfig)
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
		citusSourceOps:      hasuraOpts.CitusSourceOps,
		bigquerySourceOps:   hasuraOpts.BigQuerySourceOps,
		genericQueryRequest: hasuraOpts.GenericQueryRequest,
		pgDumpClient:        hasuraOpts.PGDumpClient,

		hasuraClient: hasuraOpts.Client,

		migrationsStateStore: hasuraOpts.MigrationsStateStore,
		settingsStateStore:   hasuraOpts.SettingsStateStore,
	}

	if err := hx.migrationsStateStore.PrepareMigrationsStateStore(hasuraOpts.SourceName); err != nil {
		logger.Debug(err)
		return nil, errors.E(op, err)
	}
	if err := hx.settingsStateStore.PrepareSettingsDriver(); err != nil {
		logger.Debug(err)
		return nil, errors.E(op, err)
	}

	return hx, nil
}

func (h *HasuraDB) Open(url string, isCMD bool, logger *log.Logger, hasuraOpts *database.HasuraOpts) (database.Driver, error) {
	var op errors.Op = "hasuradb.HasuraDB.Open"
	if logger == nil {
		logger = log.New()
	}
	hurl, err := nurl.Parse(url)
	if err != nil {
		logger.Debug(err)
		return nil, errors.E(op, err)
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
	}
	hx, err := WithInstance(config, logger, hasuraOpts)
	if err != nil {
		logger.Debug(err)
		return nil, errors.E(op, err)
	}
	return hx, nil
}

func (h *HasuraDB) Close() error {
	// nothing do to here
	return nil
}

func (h *HasuraDB) Scan() error {
	var op errors.Op = "hasuradb.HasuraDB.Scan"
	h.migrations = database.NewMigrations()
	err := h.getVersions()
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (h *HasuraDB) Lock() error {
	var op errors.Op = "hasuradb.HasuraDB.Lock"
	if h.isLocked {
		return errors.E(op, database.ErrLocked)
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
	var op errors.Op = "hasuradb.HasuraDB.Run"
	migr, err := ioutil.ReadAll(migration)
	if err != nil {
		return errors.E(op, err)
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
				return errors.E(op, err)
			}
		case hasura.SourceKindMSSQL:
			_, err := h.mssqlSourceOps.MSSQLRunSQL(hasura.MSSQLRunSQLInput(sqlInput))
			if err != nil {
				return errors.E(op, err)
			}
		case hasura.SourceKindCitus:
			_, err := h.citusSourceOps.CitusRunSQL(hasura.CitusRunSQLInput(sqlInput))
			if err != nil {
				return errors.E(op, err)
			}
		case hasura.SourceKindCockroach:
			_, err := h.cockroachSourceOps.CockroachRunSQL(hasura.CockroachRunSQLInput(sqlInput))
			if err != nil {
				return errors.E(op, err)
			}
		case hasura.SourceKindBigQuery:
			_, err := h.bigquerySourceOps.BigQueryRunSQL(hasura.BigQueryRunSQLInput(sqlInput))
			if err != nil {
				return err
			}

		default:
			return errors.E(op, fmt.Errorf("unsupported source kind, source name: %v kind: %v", h.hasuraOpts.SourceName, h.hasuraOpts.SourceKind))
		}
	case "meta":
		var metadataRequests []interface{}
		err := json.Unmarshal(migr, &metadataRequests)
		if err != nil {
			h.migrationQuery.ResetArgs()
			return errors.E(op, err)
		}
		err = sendMetadataMigrations(h, metadataRequests)
		if err != nil {
			return errors.E(op, err)
		}
		return nil
	}
	return nil
}

// responsible for deciding which request to send to v1/metadata and which to send to v2/query
func sendMetadataMigrations(hasuradb *HasuraDB, requests []interface{}) error {
	var op errors.Op = "hasuradb.sendMetadataMigrations"
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
			return errors.E(op, err)
		}
		if resp.StatusCode != http.StatusOK {
			b, err := ioutil.ReadAll(body)
			if err != nil {
				return errors.E(op, errors.KindHasuraAPI, err)
			}
			return errors.E(op, errors.KindHasuraAPI, string(b))
		}

	}
	if len(metadataRequests) > 0 {
		metadataBulk := HasuraInterfaceBulk{
			Type: "bulk",
			Args: metadataRequests,
		}
		resp, body, err := hasuradb.metadataops.SendCommonMetadataOperation(metadataBulk)
		if err != nil {
			return errors.E(op, err)
		}
		if resp.StatusCode != http.StatusOK {
			b, err := ioutil.ReadAll(body)
			if err != nil {
				return errors.E(op, errors.KindHasuraAPI, err)
			}
			return errors.E(op, errors.KindHasuraAPI, string(b))
		}
	}
	return nil
}

func (h *HasuraDB) ResetQuery() {
	h.migrationQuery.ResetArgs()
}

func (h *HasuraDB) InsertVersion(version int64) error {
	var op errors.Op = "hasuradb.HasuraDB.InsertVersion"
	err := h.migrationsStateStore.InsertVersion(h.hasuraOpts.SourceName, version)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (h *HasuraDB) SetVersion(version int64, dirty bool) error {
	var op errors.Op = "hasuradb.HasuraDB.SetVersion"
	err := h.migrationsStateStore.SetVersion(h.hasuraOpts.SourceName, version, dirty)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (h *HasuraDB) RemoveVersion(version int64) error {
	var op errors.Op = "hasuradb.HasuraDB.RemoveVersion"
	err := h.migrationsStateStore.RemoveVersion(h.hasuraOpts.SourceName, version)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (h *HasuraDB) getVersions() (err error) {
	var op errors.Op = "hasuradb.HasuraDB.getVersions"
	v, err := h.migrationsStateStore.GetVersions(h.hasuraOpts.SourceName)
	if err != nil {
		return errors.E(op, err)
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
	var op errors.Op = "hasuradb.HasuraDB.Query"
	resp, body, err := h.metadataops.SendCommonMetadataOperation(data)
	if err != nil {
		return errors.E(op, err)
	}
	if resp.StatusCode != http.StatusOK {
		b, err := ioutil.ReadAll(body)
		if err != nil {
			return errors.E(op, errors.KindHasuraAPI, err)
		}
		return errors.E(op, errors.KindHasuraAPI, string(b))
	}
	return nil
}
