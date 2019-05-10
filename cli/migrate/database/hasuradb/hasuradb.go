package hasuradb

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	nurl "net/url"
	"path"
	"regexp"
	"strconv"
	"strings"

	yaml "github.com/ghodss/yaml"
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
)

var (
	ErrNilConfig      = fmt.Errorf("no config")
	ErrNoDatabaseName = fmt.Errorf("no database name")
	ErrNoSchema       = fmt.Errorf("no schema")
	ErrDatabaseDirty  = fmt.Errorf("database is dirty")
)

type Config struct {
	MigrationsTable string
	SettingsTable   string
	v1URL           *nurl.URL
	schemDumpURL    *nurl.URL
	Headers         map[string]string
	isCMD           bool
}

type HasuraDB struct {
	config         *Config
	settings       []database.Setting
	migrations     *database.Migrations
	migrationQuery HasuraInterfaceBulk
	jsonPath       map[string]string
	isLocked       bool
	logger         *log.Logger
}

func WithInstance(config *Config, logger *log.Logger) (database.Driver, error) {
	if config == nil {
		logger.Debug(ErrNilConfig)
		return nil, ErrNilConfig
	}

	hx := &HasuraDB{
		config:     config,
		migrations: database.NewMigrations(),
		settings:   database.Settings,
		logger:     logger,
	}

	if err := hx.ensureVersionTable(); err != nil {
		logger.Debug(err)
		return nil, err
	}

	if err := hx.ensureSettingsTable(); err != nil {
		logger.Debug(err)
		return nil, err
	}

	err := hx.getVersions()
	if err != nil {
		return nil, err
	}

	return hx, nil
}

func (h *HasuraDB) Open(url string, isCMD bool, logger *log.Logger) (database.Driver, error) {
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

	hx, err := WithInstance(&Config{
		MigrationsTable: DefaultMigrationsTable,
		SettingsTable:   DefaultSettingsTable,
		v1URL: &nurl.URL{
			Scheme: scheme,
			Host:   hurl.Host,
			Path:   path.Join(hurl.Path, "v1/query"),
		},
		schemDumpURL: &nurl.URL{
			Scheme: scheme,
			Host:   hurl.Host,
			Path:   path.Join(hurl.Path, "v1alpha1/pg_dump"),
		},
		isCMD:   isCMD,
		Headers: headers,
	}, logger)

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

	if len(h.migrationQuery.Args) == 0 {
		return nil
	}

	resp, body, err := h.sendv1Query(h.migrationQuery)
	if err != nil {
		return err
	}

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return err
		}

		// Handle migration version here
		if horror.Path != "" {
			jsonData, err := json.Marshal(h.migrationQuery)
			if err != nil {
				return err
			}
			var migrationQuery interface{}
			err = json.Unmarshal(jsonData, &migrationQuery)
			if err != nil {
				return err
			}
			res, err := jsonpath.JsonPathLookup(migrationQuery, horror.Path)
			if err == nil {
				queryData, err := json.MarshalIndent(res, "", "    ")
				if err != nil {
					return err
				}
				horror.migrationQuery = string(queryData)
			}
			re1, err := regexp.Compile(`\$.args\[([0-9]+)\]*`)
			if err != nil {
				return err
			}
			result := re1.FindAllStringSubmatch(horror.Path, -1)
			if len(result) != 0 {
				migrationNumber, ok := h.jsonPath[result[0][1]]
				if ok {
					horror.migrationFile = migrationNumber
				}
			}
		}
		return horror.Error(h.config.isCMD)
	}
	h.isLocked = false
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
		t := HasuraInterfaceQuery{
			Type: "run_sql",
			Args: HasuraArgs{
				SQL: string(body),
			},
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
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable) + ` (version, dirty) VALUES (` + strconv.FormatInt(version, 10) + `, ` + fmt.Sprintf("%t", false) + `)`,
		},
	}
	h.migrationQuery.Args = append(h.migrationQuery.Args, query)
	return nil
}

func (h *HasuraDB) RemoveVersion(version int64) error {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `DELETE FROM ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable) + ` WHERE version = ` + strconv.FormatInt(version, 10),
		},
	}
	h.migrationQuery.Args = append(h.migrationQuery.Args, query)
	return nil
}

func (h *HasuraDB) getVersions() (err error) {

	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `SELECT version, dirty FROM ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable),
		},
	}

	// Send Query
	resp, body, err := h.sendv1Query(query)
	if err != nil {
		return err
	}

	var horror HasuraError

	// If status != 200 return error
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return err
		}

		return horror.Error(h.config.isCMD)
	}

	var hres HasuraSQLRes
	err = json.Unmarshal(body, &hres)
	if err != nil {
		return err
	}

	if hres.ResultType != TuplesOK {
		return fmt.Errorf("Invalid result Type %s", hres.ResultType)
	}

	if len(hres.Result) == 1 {
		return nil
	}

	for index, val := range hres.Result {
		if index == 0 {
			continue
		}

		version, err := strconv.ParseUint(val[0], 10, 64)
		if err != nil {
			return err
		}

		h.migrations.Append(version)
	}

	return nil
}

func (h *HasuraDB) Version() (version int64, dirty bool, err error) {
	tmpVersion, ok := h.migrations.Last()
	if !ok {
		return database.NilVersion, false, nil
	}

	return int64(tmpVersion), false, nil
}

func (h *HasuraDB) Reset() error {
	query := HasuraBulk{
		Type: "bulk",
		Args: []HasuraQuery{
			{
				Type: "clear_metadata",
				Args: HasuraArgs{},
			},
			{
				Type: "run_sql",
				Args: HasuraArgs{
					SQL: `DROP SCHEMA public CASCADE`,
				},
			},
			{
				Type: "run_sql",
				Args: HasuraArgs{
					SQL: `CREATE SCHEMA public`,
				},
			},
			{
				Type: "run_sql",
				Args: HasuraArgs{
					SQL: `TRUNCATE ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable),
				},
			},
		},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		return err
	}

	var horror HasuraError

	// If status != 200 return error
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return err
		}

		return horror.Error(h.config.isCMD)
	}

	return nil
}

func (h *HasuraDB) Drop() error {
	return nil
}

func (h *HasuraDB) ensureVersionTable() error {
	// check if migration table exists
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + h.config.MigrationsTable + `' AND table_schema = '` + DefaultSchema + `' LIMIT 1`,
		},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	var horror HasuraError

	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return err
		}
		return horror.Error(h.config.isCMD)
	}

	var hres HasuraSQLRes

	err = json.Unmarshal(body, &hres)
	if err != nil {
		h.logger.Debug(err)
		return err
	}

	if hres.ResultType != TuplesOK {
		return fmt.Errorf("Invalid result Type %s", hres.ResultType)
	}

	if hres.Result[1][0] != "0" {
		return nil
	}

	// Now Create the table
	query = HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `CREATE TABLE ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable) + ` (version bigint not null primary key, dirty boolean not null)`,
		},
	}

	resp, body, err = h.sendv1Query(query)
	if err != nil {
		return err
	}

	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return err
		}

		return horror.Error(h.config.isCMD)
	}

	err = json.Unmarshal(body, &hres)
	if err != nil {
		return err
	}

	if hres.ResultType != CommandOK {
		return fmt.Errorf("Creating Version table failed %s", hres.ResultType)
	}

	return nil
}

func (h *HasuraDB) sendv1Query(m interface{}) (resp *http.Response, body []byte, err error) {
	request := gorequest.New()

	request = request.Post(h.config.v1URL.String()).Send(m)

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
	request := gorequest.New()

	request = request.Post(h.config.schemDumpURL.String()).Send(m)

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

func (h *HasuraDB) First() (version uint64, ok bool) {
	return h.migrations.First()
}

func (h *HasuraDB) Last() (version uint64, ok bool) {
	return h.migrations.Last()
}

func (h *HasuraDB) Prev(version uint64) (prevVersion uint64, ok bool) {
	return h.migrations.Prev(version)
}

func (h *HasuraDB) Next(version uint64) (nextVersion uint64, ok bool) {
	return h.migrations.Next(version)
}

func (h *HasuraDB) Read(version uint64) (ok bool) {
	return h.migrations.Read(version)
}
