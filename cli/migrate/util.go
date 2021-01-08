package migrate

import "C"
import (
	"fmt"
	nurl "net/url"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"

	"github.com/hasura/graphql-engine/cli/internal/client"

	"github.com/hasura/graphql-engine/cli/migrate/database"

	crontriggers "github.com/hasura/graphql-engine/cli/metadata/cron_triggers"

	"github.com/hasura/graphql-engine/cli/metadata"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/metadata/allowlist"
	"github.com/hasura/graphql-engine/cli/metadata/functions"
	"github.com/hasura/graphql-engine/cli/metadata/querycollections"
	"github.com/hasura/graphql-engine/cli/metadata/remoteschemas"
	"github.com/hasura/graphql-engine/cli/metadata/sources"
	"github.com/hasura/graphql-engine/cli/metadata/tables"
	"github.com/hasura/graphql-engine/cli/metadata/types"
	"github.com/hasura/graphql-engine/cli/metadata/version"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
)

// MultiError holds multiple errors.
type MultiError struct {
	Errs []error
}

// NewMultiError returns an error type holding multiple errors.
func NewMultiError(errs ...error) MultiError {
	compactErrs := make([]error, 0)
	for _, e := range errs {
		if e != nil {
			compactErrs = append(compactErrs, e)
		}
	}
	return MultiError{compactErrs}
}

// Error implements error. Mulitple errors are concatenated with 'and's.
func (m MultiError) Error() string {
	var strs = make([]string, 0)
	for _, e := range m.Errs {
		if len(e.Error()) > 0 {
			strs = append(strs, e.Error())
		}
	}
	return strings.Join(strs, " and ")
}

// suint64 safely converts int to uint64
// see https://goo.gl/wEcqof
// see https://goo.gl/pai7Dr
func suint64(n int64) uint64 {
	if n < 0 {
		panic(fmt.Sprintf("suint(%v) expects input >= 0", n))
	}
	return uint64(n)
}

/*
// newSlowReader turns an io.ReadCloser into a slow io.ReadCloser.
// Use this to simulate a slow internet connection.
func newSlowReader(r io.ReadCloser) io.ReadCloser {
	return &slowReader{
		rx:     r,
		reader: bufio.NewReader(r),
	}
}

type slowReader struct {
	rx     io.ReadCloser
	reader *bufio.Reader
}

func (b *slowReader) Read(p []byte) (n int, err error) {
	time.Sleep(10 * time.Millisecond)
	c, err := b.reader.ReadByte()
	if err != nil {
		return 0, err
	} else {
		copy(p, []byte{c})
		return 1, nil
	}
}

func (b *slowReader) Close() error {
	return b.rx.Close()
} */

var errNoScheme = fmt.Errorf("no scheme")

// schemeFromUrl returns the scheme from a URL string
func schemeFromUrl(url string) (string, error) {
	u, err := nurl.Parse(url)
	if err != nil {
		return "", err
	}

	if len(u.Scheme) == 0 {
		return "", errNoScheme
	}

	return u.Scheme, nil
}

// FilterCustomQuery filters all query values starting with `x-`
func FilterCustomQuery(u *nurl.URL) *nurl.URL {
	ux := *u
	vx := make(nurl.Values)
	for k, v := range ux.Query() {
		if len(k) <= 1 || (len(k) > 1 && k[0:2] != "x-") {
			vx[k] = v
		}
	}
	ux.RawQuery = vx.Encode()
	return &ux
}

func NewMigrate(ec *cli.ExecutionContext, isCmd bool, datasource string) (*Migrate, error) {
	dbURL := GetDataPath(ec)
	fileURL := GetFilePath(filepath.Join(ec.MigrationDir, datasource))
	opts := NewMigrateOpts{
		fileURL.String(),
		dbURL.String(),
		isCmd, int(ec.Config.Version),
		ec.Config.ServerConfig.TLSConfig,
		ec.Logger,
		&database.HasuraOpts{
			ServerFeatureFlags: *ec.Version.ServerFeatureFlags,
			Datasource:         datasource,
			Client:             ec.APIClient,
		},
	}

	if ec.Version.ServerFeatureFlags.HasDatasources && (ec.Config.Version > cli.V1) {
		defaultDatasourceName := "default"
		// check if migration table exists and migrate state to catalog state API
		hasuraAPIClient, err := client.NewHasuraRestAPIClient(client.NewHasuraRestAPIClientOpts{
			Headers:        ec.HGEHeaders,
			QueryAPIURL:    fmt.Sprintf("%s/%s", ec.Config.Endpoint, "v2/query"),
			MetadataAPIURL: fmt.Sprintf("%s/%s", ec.Config.Endpoint, "v1/metadata"),
			TLSConfig:      ec.Config.TLSConfig,
		})
		shouldMigrateStateToCatalogState := false
		isSettingsStateMoved, err := hasuraAPIClient.CheckIfSettingsStateWasMovedToCatalogState()
		if err != nil {
			ec.Logger.Debug("checking if settings state was moved: ", err)
		}
		isMigrationStateMoved, err := hasuraAPIClient.CheckIfMigrationStateStoreWasMovedToCatalogState()
		if err != nil {
			ec.Logger.Debug("checking if migration state was moved: ", err)
		}
		shouldMigrateStateToCatalogState = !isSettingsStateMoved && !isMigrationStateMoved
		if shouldMigrateStateToCatalogState {
			migrations, err := hasuraAPIClient.GetMigrationVersions(hasuradb.DefaultSchema, hasuradb.DefaultMigrationsTable)
			if err != nil {
				ec.Logger.Debug("getting migration versions from schema_migrations table: ", err)
			}
			settings, err := hasuraAPIClient.GetCLISettingsFromSQLTable(hasuradb.DefaultSchema, hasuradb.DefaultSettingsTable)
			if err != nil {
				ec.Logger.Debug("getting migration settings: ", err)
			}
			if err := hasuraAPIClient.MoveMigrationsAndSettingsToCatalogState(defaultDatasourceName, migrations, settings); err != nil {
				ec.Logger.Debug("migrating CLI state to catalog API: ", err)
			}

			// mark both these tables as moved
			if err := hasuraAPIClient.MarkCLIStateTablesAsMovedToCatalogState(); err != nil {
				ec.Logger.Debug("marking cli state was moved from tables to catalog state API: ", err)
			}
		}
	}
	if ec.Version.ServerFeatureFlags.HasDatasources && (ec.Config.Version > cli.V1) {
		opts.hasuraOpts.APIVersion = client.V2API
	} else {
		opts.hasuraOpts.APIVersion = client.V1API
	}
	t, err := New(opts)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}
	// Set Plugins
	SetMetadataPluginsWithDir(ec, t)
	if ec.Config.Version >= cli.V2 {
		t.EnableCheckMetadataConsistency(true)
	}
	return t, nil
}

func GetDataPath(ec *cli.ExecutionContext) *nurl.URL {
	url := ec.Config.ServerConfig.ParsedEndpoint
	host := &nurl.URL{
		Scheme:   "hasuradb",
		Host:     url.Host,
		Path:     url.Path,
		RawQuery: ec.Config.ServerConfig.APIPaths.GetQueryParams().Encode(),
	}
	q := host.Query()
	// Set sslmode in query
	switch scheme := url.Scheme; scheme {
	case "https":
		q.Set("sslmode", "enable")
	default:
		q.Set("sslmode", "disable")
	}
	for k, v := range ec.HGEHeaders {
		q.Add("headers", fmt.Sprintf("%s:%s", k, v))
	}
	host.RawQuery = q.Encode()
	return host
}

func SetMetadataPluginsWithDir(ec *cli.ExecutionContext, drv *Migrate, dir ...string) {
	var metadataDir string
	if len(dir) == 0 {
		metadataDir = ec.MetadataDir
	} else {
		metadataDir = dir[0]
	}
	ec.Version.GetServerFeatureFlags()
	plugins := make(types.MetadataPlugins, 0)
	if ec.Config.Version == cli.V2 && metadataDir != "" {
		plugins = append(plugins, version.New(ec, metadataDir))
		plugins = append(plugins, querycollections.New(ec, metadataDir))
		plugins = append(plugins, allowlist.New(ec, metadataDir))
		plugins = append(plugins, remoteschemas.New(ec, metadataDir))
		plugins = append(plugins, actions.New(ec, metadataDir))
		plugins = append(plugins, crontriggers.New(ec, metadataDir))

		if ec.Version.ServerFeatureFlags.HasDatasources {
			plugins = append(plugins, sources.New(ec, metadataDir))
		} else {
			plugins = append(plugins, tables.New(ec, metadataDir))
			plugins = append(plugins, functions.New(ec, metadataDir))
		}
	} else {
		plugins = append(plugins, metadata.New(ec, ec.MigrationDir))
	}
	drv.SetMetadataPlugins(plugins)
}

func GetFilePath(dir string) *nurl.URL {
	host := &nurl.URL{
		Scheme: "file",
		Path:   dir,
	}

	// Add Prefix / to path if runtime.GOOS equals to windows
	if runtime.GOOS == "windows" && !strings.HasPrefix(host.Path, "/") {
		host.Path = "/" + host.Path
	}
	return host
}
