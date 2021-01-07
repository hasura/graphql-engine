package hasuradb

import (
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	nurl "net/url"
	"path"
	"regexp"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/client"

	"github.com/pkg/errors"

	"github.com/mitchellh/mapstructure"

	yaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/metadata/types"
	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/oliveagle/jsonpath"
	"github.com/parnurzeal/gorequest"
	log "github.com/sirupsen/logrus"
)

func init() {
	db := HasuraDB{}
	database.Register("hasuradb", &db)
}

const (
	DefaultMigrationsTable = "schema_migrations"
	DefaultSchema          = "hdb_catalog"

	DefaultDatasourceName = "default"
)

var (
	ErrNilConfig      = fmt.Errorf("no config")
	ErrNoDatabaseName = fmt.Errorf("no database name")
	ErrNoSchema       = fmt.Errorf("no schema")
	ErrDatabaseDirty  = fmt.Errorf("database is dirty")

	queryTypes    = []string{"select", "insert", "select", "update", "delete", "count", "run_sql", "bulk"}
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
	Plugins                        types.MetadataPlugins
	enableCheckMetadataConsistency bool
	Req                            *gorequest.SuperAgent
}

type HasuraDB struct {
	config         *Config
	settings       []database.Setting
	migrations     *database.Migrations
	migrationQuery HasuraInterfaceBulk
	jsonPath       map[string]string
	isLocked       bool
	logger         *log.Logger
	hasuraOpts     *database.HasuraOpts

	CLICatalogState *client.CLICatalogState

	migrationStateStore   MigrationsStateStore
	settingsStateStore    SettingsStateStore
	MetadataOrQueryClient client.MetadataOrQueryClientFunc
	client                *client.Client
}

func (h *HasuraDB) SendMetadataOrQueryRequest(m interface{}, opts *client.MetadataOrQueryClientFuncOpts) (*http.Response, []byte, error) {
	return h.MetadataOrQueryClient(m, opts, client.Config{
		QueryURL:    h.config.queryURL,
		MetadataURL: h.config.metadataURL,
		GraphqlURL:  h.config.graphqlURL,
		PGDumpURL:   h.config.pgDumpURL,
		Req:         h.config.Req,
		Headers:     h.config.Headers,
	})
}
func WithInstance(config *Config, logger *log.Logger, hasuraOpts *database.HasuraOpts) (database.Driver, error) {
	if config == nil {
		logger.Debug(ErrNilConfig)
		return nil, ErrNilConfig
	}

	hx := &HasuraDB{
		config:     config,
		migrations: database.NewMigrations(),
		settings:   database.Settings,
		logger:     logger,
		hasuraOpts: hasuraOpts,
		client:     hasuraOpts.Client,
	}
	switch {
	case hasuraOpts.APIVersion == client.V2API:
		if hx.hasuraOpts.Datasource == "" {
			hx.hasuraOpts.Datasource = DefaultDatasourceName
		}
		hx.CLICatalogState = new(client.CLICatalogState)
		hx.CLICatalogState.Migrations = client.MigrationsState{}
		hx.migrationStateStore = NewMigrationsStateWithCatalogStateAPI(hx)
		hx.settingsStateStore = NewSettingsStateStoreWithCatalogStateAPI(hx)
		hx.MetadataOrQueryClient = hx.client.SendV2QueryOrV1Metadata
	default:
		hx.migrationStateStore = NewMigrationStateStoreWithSQL(hx)
		hx.settingsStateStore = NewSettingsStateStoreWithSQL(hx)
		hx.MetadataOrQueryClient = hx.client.Sendv1Query
	}

	if err := hx.migrationStateStore.PrepareMigrationsStateStore(); err != nil {
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
		MigrationsTable: DefaultMigrationsTable,
		SettingsTable:   DefaultSettingsTable,
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
		Plugins: make(types.MetadataPlugins, 0),
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

func (h *HasuraDB) UnLockSeq() error {
	if !h.isLocked {
		return nil
	}

	defer func() {
		h.isLocked = false
	}()
	return nil
}

func (h *HasuraDB) RunSeq(migration io.Reader, fileType, fileName string) error {
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
		sqlInput := RunSQLInput{
			SQL: string(body),
		}
		if h.config.enableCheckMetadataConsistency {
			sqlInput.CheckMetadataConsistency = func() *bool { b := false; return &b }()
		}
		t := HasuraInterfaceQuery{
			Type: RunSQL,
			Args: sqlInput,
		}
		resp, body, err := h.SendMetadataOrQueryRequest(t, &client.MetadataOrQueryClientFuncOpts{QueryRequestOpts: &client.QueryRequestOpts{}})
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			return errors.New(string(body))
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

func sendMetadataMigrations(hasuradb *HasuraDB, metadataRequests []interface{}) error {
	for _, v := range metadataRequests {
		type bulkQuery struct {
			Type string        `mapstructure:"type"`
			Args []interface{} `mapstructure:"args"`
		}
		var bulk = new(bulkQuery)
		if _ = mapstructure.Decode(v, bulk); bulk.Type == "bulk" {
			if err := sendMetadataMigrations(hasuradb, bulk.Args); err != nil {
				return err
			}
			// was a bulk query and was already handled above so skip this iteration
			continue
		}
		resp, body, err := hasuradb.SendMetadataOrQueryRequest(v, nil)
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			return errors.New(string(body))
		}
	}
	return nil
}

func (h *HasuraDB) UnLock() error {
	if !h.isLocked {
		return nil
	}

	defer func() {
		h.isLocked = false
	}()

	if len(h.migrationQuery.Args) == 0 {
		return nil
	}
	// can have both metadata and query api requests in h.migrationQuery
	// this has to
	if h.hasuraOpts.APIVersion >= client.V2API {
		var reqs []interface{}
		var metadataAPIRequests []interface{}
		var queryAPIRequests []interface{}
		if err := mapstructure.Decode(h.migrationQuery.Args, &reqs); err != nil {
			return errors.Wrap(err, "constructing list of API calls")
		}
		for _, req := range reqs {
			var r map[string]interface{}
			if err := mapstructure.Decode(req, &r); err != nil {
				return err
			}
			v, ok := r["type"]
			if !ok {
				v, ok = r["Type"]
			}
			if ok {
				if w := fmt.Sprintf("%v", v); w != "" {
					if _, ok := queryTypesMap[w]; ok {
						queryAPIRequests = append(queryAPIRequests, req)
					} else {
						metadataAPIRequests = append(metadataAPIRequests, req)
					}
				}
			}
		}
		resp, body, err := h.SendMetadataOrQueryRequest(HasuraInterfaceQuery{
			Type:   "bulk",
			Source: h.hasuraOpts.Datasource,
			Args:   queryAPIRequests,
		}, &client.MetadataOrQueryClientFuncOpts{QueryRequestOpts: &client.QueryRequestOpts{}})
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			switch herror := NewHasuraError(body, h.config.isCMD).(type) {
			case HasuraError:
				// Handle migration version here
				if herror.Path != "" {
					jsonData, err := json.Marshal(h.migrationQuery)
					if err != nil {
						return err
					}
					var migrationQuery interface{}
					err = json.Unmarshal(jsonData, &migrationQuery)
					if err != nil {
						return err
					}
					res, err := jsonpath.JsonPathLookup(migrationQuery, herror.Path)
					if err == nil {
						queryData, err := json.MarshalIndent(res, "", "    ")
						if err != nil {
							return err
						}
						herror.migrationQuery = string(queryData)
					}
					re1, err := regexp.Compile(`\$.args\[([0-9]+)\]*`)
					if err != nil {
						return err
					}
					result := re1.FindAllStringSubmatch(herror.Path, -1)
					if len(result) != 0 {
						migrationNumber, ok := h.jsonPath[result[0][1]]
						if ok {
							herror.migrationFile = migrationNumber
						}
					}
				}
				return herror
			default:
				return herror
			}
		}
		resp, body, err = h.SendMetadataOrQueryRequest(HasuraInterfaceQuery{
			Type: "bulk",
			Args: metadataAPIRequests,
		}, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			switch herror := NewHasuraError(body, h.config.isCMD).(type) {
			case HasuraError:
				// Handle migration version here
				if herror.Path != "" {
					jsonData, err := json.Marshal(h.migrationQuery)
					if err != nil {
						return err
					}
					var migrationQuery interface{}
					err = json.Unmarshal(jsonData, &migrationQuery)
					if err != nil {
						return err
					}
					res, err := jsonpath.JsonPathLookup(migrationQuery, herror.Path)
					if err == nil {
						queryData, err := json.MarshalIndent(res, "", "    ")
						if err != nil {
							return err
						}
						herror.migrationQuery = string(queryData)
					}
					re1, err := regexp.Compile(`\$.args\[([0-9]+)\]*`)
					if err != nil {
						return err
					}
					result := re1.FindAllStringSubmatch(herror.Path, -1)
					if len(result) != 0 {
						migrationNumber, ok := h.jsonPath[result[0][1]]
						if ok {
							herror.migrationFile = migrationNumber
						}
					}
				}
				return herror
			default:
				return herror
			}
		}

	} else {
		resp, body, err := h.SendMetadataOrQueryRequest(h.migrationQuery, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
		if err != nil {
			return err
		}

		if resp.StatusCode != http.StatusOK {
			switch herror := NewHasuraError(body, h.config.isCMD).(type) {
			case HasuraError:
				// Handle migration version here
				if herror.Path != "" {
					jsonData, err := json.Marshal(h.migrationQuery)
					if err != nil {
						return err
					}
					var migrationQuery interface{}
					err = json.Unmarshal(jsonData, &migrationQuery)
					if err != nil {
						return err
					}
					res, err := jsonpath.JsonPathLookup(migrationQuery, herror.Path)
					if err == nil {
						queryData, err := json.MarshalIndent(res, "", "    ")
						if err != nil {
							return err
						}
						herror.migrationQuery = string(queryData)
					}
					re1, err := regexp.Compile(`\$.args\[([0-9]+)\]*`)
					if err != nil {
						return err
					}
					result := re1.FindAllStringSubmatch(herror.Path, -1)
					if len(result) != 0 {
						migrationNumber, ok := h.jsonPath[result[0][1]]
						if ok {
							herror.migrationFile = migrationNumber
						}
					}
				}
				return herror
			default:
				return herror
			}
		}
	}
	return nil
}
func (h *HasuraDB) Run(migration io.Reader, fileType, fileName string) error {
	// have 2 cases
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
		sqlInput := RunSQLInput{
			SQL: string(body),
		}
		if h.config.enableCheckMetadataConsistency {
			sqlInput.CheckMetadataConsistency = func() *bool { b := false; return &b }()
		}
		t := HasuraInterfaceQuery{
			Type: RunSQL,
			Args: sqlInput,
		}
		h.migrationQuery.Args = append(h.migrationQuery.Args, t)
		h.jsonPath[fmt.Sprintf("%d", len(h.migrationQuery.Args)-1)] = fileName
	case "meta":
		var t []interface{}
		err := yaml.Unmarshal(migr, &t)
		if err != nil {
			h.migrationQuery.ResetArgs()
			return err
		}

		for _, v := range t {
			h.migrationQuery.Args = append(h.migrationQuery.Args, v)
			h.jsonPath[fmt.Sprintf("%d", len(h.migrationQuery.Args)-1)] = fileName
		}
	}
	return nil
}

func (h *HasuraDB) ResetQuery() {
	h.migrationQuery.ResetArgs()
}

func (h *HasuraDB) InsertVersion(version int64) error {
	return h.migrationStateStore.InsertVersion(version)
}

func (h *HasuraDB) SetVersion(version int64, dirty bool) error {
	return h.migrationStateStore.SetVersion(version, dirty)
}

func (h *HasuraDB) RemoveVersion(version int64) error {
	return h.migrationStateStore.RemoveVersion(version)
}

func (h *HasuraDB) getVersions() (err error) {
	return h.migrationStateStore.GetVersions()
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

func (h *HasuraDB) sendv1Query(m interface{}, _ client.MetadataOrQueryClientFuncOpts) (resp *http.Response, body []byte, err error) {
	request := h.config.Req.Clone()
	request = request.Post(h.config.queryURL.String()).Send(m)
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

func (h *HasuraDB) sendV2QueryOrV1Metadata(m interface{}, opts client.MetadataOrQueryClientFuncOpts) (resp *http.Response, body []byte, err error) {
	var endpoint string
	switch {
	case h.hasuraOpts.APIVersion == client.V1API:
		endpoint = h.config.queryURL.String()
	default:
		// TODO: Make this better
		if opts.MetadataRequestOpts != nil {
			endpoint = h.config.metadataURL.String()
			break
		}
		if opts.QueryRequestOpts != nil {
			endpoint = h.config.queryURL.String()
			break
		}
		if endpoint == "" {
			var v map[string]interface{}
			if err := mapstructure.Decode(m, &v); err != nil {
				return nil, nil, fmt.Errorf("unmarshalling request body failed")
			} else {
				requestType := fmt.Sprintf("%v", v["Type"])
				if _, ok := queryTypesMap[requestType]; ok {
					endpoint = h.config.queryURL.String()
				} else {
					endpoint = h.config.metadataURL.String()
				}
			}
		}

	}

	request := h.config.Req.Clone()
	request = request.Post(endpoint).Send(m)
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

func (h *HasuraDB) sendv1GraphQL(query interface{}) (resp *http.Response, body []byte, err error) {
	request := h.config.Req.Clone()
	request = request.Post(h.config.graphqlURL.String()).Send(query)

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
