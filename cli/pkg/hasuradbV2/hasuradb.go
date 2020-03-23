package hasuradbV2

// This package will implement the github.com/golang-migrate/migrate/database.Driver interface
// Along with a bunch of methods which are specifically required for hasura
import (
	"io"
	"net/url"
	"path"
	"strings"

	v1 "github.com/hasura/graphql-engine/cli/client/v1"

	"github.com/golang-migrate/migrate/v4/database"
	"github.com/hasura/graphql-engine/cli/metadata/types"
)

const (
	defaultMigrationsTable = "schema_migrations"
	defaultSettingsTable   = "migration_settings"
	defaultSchema          = "hdb_catalog"
)

type Config struct {
	MigrationsTable string
	SettingsTable   string
	v1URL           *url.URL
	graphqlURL      *url.URL
	schemaDumpURL   *url.URL
	Headers         map[string]string
	isCMD           bool
	Plugins         types.MetadataPlugins
}

type DB struct {
	config   *Config
	v1Client *v1.Client
	isLocked bool
}

func WithInstance(instance *DB, config *Config) (database.Driver, error) {
	if config == nil {
		return nil, v1.ErrNilConfig
	}

	// TODO: Find what this does
	//hx := &DB{
	//	config:     config,
	//	migrations: database.NewMigrations(),
	//	settings:   database.Settings,
	//	logger:     logger,
	//}
	instance.config = config

	if err := instance.ensureVersionTable(); err != nil {
		return nil, err
	}
	return instance, nil
}

func (db *DB) Open(hasuraURL string) (database.Driver, error) {
	// TODO: Abstract logger
	parsedHasuraURL, err := url.Parse(hasuraURL)
	if err != nil {
		return nil, err
	}
	// Use sslMode query param to set Scheme
	var scheme string
	params := parsedHasuraURL.Query()
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
		MigrationsTable: defaultMigrationsTable,
		SettingsTable:   defaultSettingsTable,
		v1URL: &url.URL{
			Scheme: scheme,
			Host:   parsedHasuraURL.Host,
			Path:   path.Join(parsedHasuraURL.Path, "v1/query"),
		},
		graphqlURL: &url.URL{
			Scheme: scheme,
			Host:   parsedHasuraURL.Host,
			Path:   path.Join(parsedHasuraURL.Path, "v1/graphql"),
		},
		schemaDumpURL: &url.URL{
			Scheme: scheme,
			Host:   parsedHasuraURL.Host,
			Path:   path.Join(parsedHasuraURL.Path, "v1alpha1/pg_dump"),
		},
		//TODO: Abstract this
		//isCMD:   isCMD,
		Headers: headers,
		Plugins: make(types.MetadataPlugins, 0),
	}
	hasuraDB, err := WithInstance(&DB{}, config)
	if err != nil {
		return nil, err
	}
	return hasuraDB, nil
}

func (db *DB) Close() error {
	panic("implement me")
}

func (db *DB) Lock() error {
	panic("implement me")
}

func (db *DB) Unlock() error {
	panic("implement me")
}

func (db *DB) Run(migration io.Reader) error {
	panic("implement me")
}

func (db *DB) SetVersion(version int, dirty bool) error {
	panic("implement me")
}

func (db *DB) Version() (version int, dirty bool, err error) {
	panic("implement me")
}

func (db *DB) Drop() error {
	panic("implement me")
}

func init() {
	db := &DB{}
	database.Register("hasura", db)
}
