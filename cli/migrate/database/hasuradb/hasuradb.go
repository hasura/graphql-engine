package hasuradb

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	nurl "net/url"
	"regexp"
	"strconv"
	"strings"

	"github.com/Masterminds/semver"
	yaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database"
	log "github.com/sirupsen/logrus"
	"github.com/parnurzeal/gorequest"
)

func init() {
	db := HasuraDB{}
	database.Register("hasuradb", &db)
}

const (
	DefaultMigrationsTable = "schema_migrations"
	DefaultRole            = "admin"
	DefaultUserID          = "0"
	DefaultSchema          = "hdb_catalog"
	ACCESS_KEY_HEADER      = "X-Hasura-Access-Key"
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
	URL             *nurl.URL
	Role            string
	UserID          string
	isCMD           bool
}

type HasuraDB struct {
	config         *Config
	settings       []database.Setting
	migrations     *database.Migrations
	migrationQuery HasuraInterfaceBulk
	isLocked       bool
}

func parsePlatformVersion(version string) (*semver.Version, error) {
	platformVersion, err := semver.NewVersion(strings.TrimPrefix(version, "v"))
	if err != nil {
		return nil, err
	}
	return platformVersion, nil
}

func WithInstance(config *Config) (database.Driver, error) {
	if config == nil {
		log.Debug(ErrNilConfig)
		return nil, ErrNilConfig
	}

	hx := &HasuraDB{
		config:     config,
		migrations: database.NewMigrations(),
		settings:   database.Settings,
	}

	if err := hx.ensureVersionTable(); err != nil {
		log.Debug(err)
		return nil, err
	}

	if err := hx.ensureSettingsTable(); err != nil {
		log.Debug(err)
		return nil, err
	}

	err := hx.getVersions()
	if err != nil {
		return nil, err
	}

	return hx, nil
}

func (h *HasuraDB) Open(url string, isCMD bool) (database.Driver, error) {
	hurl, err := nurl.Parse(url)
	if err != nil {
		log.Debug(err)
		return nil, err
	}

	var user, pass string
	switch hurl.User {
	case nil:
		user = DefaultRole
		pass = DefaultUserID
	default:
		user = hurl.User.Username()
		if user == "" {
			user = DefaultRole
		}
		tmpPass, ok := hurl.User.Password()
		if !ok {
			// If Pass not set
			pass = DefaultUserID
		} else {
			pass = tmpPass
		}
	}

	hx, err := WithInstance(&Config{
		MigrationsTable: DefaultMigrationsTable,
		SettingsTable:   DefaultSettingsTable,
		URL:             hurl,
		Role:            user,
		UserID:          pass,
		isCMD:           isCMD,
	})

	if err != nil {
		log.Debug(err)
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

	resp, body, err := h.sendQuery(h.migrationQuery)
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
			re1, err := regexp.Compile(`\$.args\[([0-9]+)\]*`)
			if err != nil {
				return err
			}

			result := re1.FindAllStringSubmatch(horror.Path, -1)
			if len(result) != 0 {

			}
		}
		return horror.Error(h.config.isCMD)
	}
	h.isLocked = false
	return nil
}

func (h *HasuraDB) Run(migration io.Reader, fileType string) error {
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

	case "meta":
		var t []interface{}
		err := yaml.Unmarshal(migr, &t)
		if err != nil {
			h.migrationQuery.ResetArgs()
			return err
		}

		for _, v := range t {
			h.migrationQuery.Args = append(h.migrationQuery.Args, v)
		}
	}
	return nil
}

func (h *HasuraDB) ResetQuery() {
	h.migrationQuery.ResetArgs()
}

func (h *HasuraDB) InsertVersion(version int) error {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable) + ` (version, dirty) VALUES (` + strconv.Itoa(version) + `, ` + fmt.Sprintf("%t", false) + `)`,
		},
	}
	h.migrationQuery.Args = append(h.migrationQuery.Args, query)
	return nil
}

func (h *HasuraDB) RemoveVersion(version int) error {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `DELETE FROM ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.MigrationsTable) + ` WHERE version = ` + strconv.Itoa(version),
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
	resp, body, err := h.sendQuery(query)
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

	resp, body, err := h.sendQuery(query)
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

	resp, body, err := h.sendQuery(query)
	if err != nil {
		log.Debug(err)
		return err
	}
	log.Debug("response: ", string(body))

	var horror HasuraError

	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			log.Debug(err)
			return err
		}
		return horror.Error(h.config.isCMD)
	}

	var hres HasuraSQLRes

	err = json.Unmarshal(body, &hres)
	if err != nil {
		log.Debug(err)
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

	resp, body, err = h.sendQuery(query)
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

func (h *HasuraDB) sendQuery(m interface{}) (resp *http.Response, body []byte, err error) {
	request := gorequest.New()

	newURL := h.config.URL

	newURL.Scheme = "http"
	newURL.User = nil

	if !strings.Contains(newURL.Path, "v1/query") {
		newURL.Path = SingleJoiningSlash(newURL.Path, "v1/query")
	}

	request = request.Post(newURL.String()).Send(m)

	if h.config.UserID != "" {
		request = request.Set(ACCESS_KEY_HEADER, h.config.UserID)
	}

	resp, body, errs := request.EndBytes()

	if len(errs) == 0 {
		err = nil
	} else {
		err = errs[0]
	}

	return resp, body, err
}

func SingleJoiningSlash(a, b string) string {
	aslash := strings.HasSuffix(a, "/")
	bslash := strings.HasPrefix(b, "/")
	switch {
	case aslash && bslash:
		return a + b[1:]
	case !aslash && !bslash:
		return a + "/" + b
	}
	return a + b
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
