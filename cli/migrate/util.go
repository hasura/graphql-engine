package migrate

import (
	"fmt"
	"github.com/hasura/graphql-engine/cli/internal/scripts"
	"github.com/hasura/graphql-engine/cli/internal/statestore"
	"github.com/hasura/graphql-engine/cli/internal/statestore/migrations"
	nurl "net/url"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	migratedb "github.com/hasura/graphql-engine/cli/migrate/database"

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

func NewMigrate(ec *cli.ExecutionContext, isCmd bool, sourceName string, sourceKind hasura.SourceKind) (*Migrate, error) {
	// set a default source kind
	if len(sourceKind) < 1 {
		return nil, fmt.Errorf("invalid source kind")
	}
	// create a new directory for the database if it doesn't exists
	if f, _ := os.Stat(filepath.Join(ec.MigrationDir, sourceName)); f == nil {
		err := os.MkdirAll(filepath.Join(ec.MigrationDir, sourceName), 0755)
		if err != nil {
			return nil, err
		}
	}
	dbURL := GetDataPath(ec)
	fileURL := GetFilePath(filepath.Join(ec.MigrationDir, sourceName))
	opts := NewMigrateOpts{
		fileURL.String(),
		dbURL.String(),
		isCmd, int(ec.Config.Version),
		ec.Config.ServerConfig.TLSConfig,
		ec.Logger,
		&migratedb.HasuraOpts{
			HasMetadataV3: ec.HasMetadataV3,
			SourceName:    sourceName,
			SourceKind:    sourceKind,
			Client:        ec.APIClient,
			V2MetadataOps: func() hasura.V2CommonMetadataOperations {
				if ec.Config.Version >= cli.V3 {
					return ec.APIClient.V1Metadata
				}
				return nil
			}(),
			MetadataOps:          cli.GetCommonMetadataOps(ec),
			MigrationsStateStore: cli.GetMigrationsStateStore(ec),
			SettingsStateStore:   cli.GetSettingsStateStore(ec),
		},
	}
	if ec.HasMetadataV3 {
		opts.hasuraOpts.PGSourceOps = ec.APIClient.V2Query
		opts.hasuraOpts.MSSQLSourceOps = ec.APIClient.V2Query
		opts.hasuraOpts.GenericQueryRequest = ec.APIClient.V2Query.Send
	} else {
		opts.hasuraOpts.PGSourceOps = ec.APIClient.V1Query
		opts.hasuraOpts.GenericQueryRequest = ec.APIClient.V1Query.Send
	}

	t, err := New(opts)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}
	if ec.Config.Version >= cli.V2 {
		t.databaseDrv.EnableCheckMetadataConsistency(true)
	}
	if ok, err := copyStateToCatalogStateAPIIfRequired(ec, sourceName); err != nil {
		ec.Logger.Warn(err)
	} else if ok {
		if err := t.ReScan(); err != nil {
			return nil, err
		}
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

func IsMigrationsSupported(kind hasura.SourceKind) bool {
	switch kind {
	case hasura.SourceKindMSSQL, hasura.SourceKindPG:
		return true
	}
	return false
}

func copyStateToCatalogStateAPIIfRequired(ec *cli.ExecutionContext, sourceName string) (bool, error) {
	// if
	//		the project is in config v3
	// 		source name is default
	// 		isStateCopyCompleted is false in catalog state
	//		hdb_catalog.schema_migrations is not empty
	if !ec.DisableAutoStateMigration && ec.Config.Version >= cli.V3 && sourceName == "default" {
		// get cli catalog and check isStateCopyCompleted is false
		cs := statestore.NewCLICatalogState(ec.APIClient.V1Metadata)
		state, err := cs.Get()
		if err != nil {
			return false, err
		}
		markStateMigrationCompleted := func() error {
			state.IsStateCopyCompleted = true
			if _, err := cs.Set(*state); err != nil {
				return fmt.Errorf("error settting state: %w", err)
			}
			return nil
		}
		if !state.IsStateCopyCompleted {
			// if control reaches this block we'll set IsStateCopyCompleted to true
			// this makes sure we only attempt to automatically do the state migration once
			// we'll leave it up to the user to correct the errors and use
			// scripts update-project-v3 --move-state-only to move state
			//
			// this will also make sure new config v3 projects will not repeatedly reach this block
			// for a example a user connecting a custom source named default
			// with no read permissions to other schemas ie we cannot access `hdb_catalog.schema_migrations`
			// in the first run it'll encounter an error but will also mark IsStateCopyCompleted to true
			// thereby not running this block again

			// check if hdb_catalog.schema_migrations exists
			// check if migrations state table exists
			query := hasura.PGRunSQLInput{
				SQL: `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + migrations.DefaultMigrationsTable + `' AND table_schema = '` + migrations.DefaultSchema + `' LIMIT 1`,
			}

			runsqlResp, err := ec.APIClient.V2Query.PGRunSQL(query)
			if err != nil {
				ec.Logger.Warn("encountered error when trying to move migrations from hdb_catalog.schema_migrations to catalog state\n", err,
					"\nnote: ignore this if you are not updating your project from config v2 -> config v3")
				ec.Logger.Debug("marking IsStateCopyCompleted as true %w", markStateMigrationCompleted())
				return false, nil
			}

			if runsqlResp.ResultType != hasura.TuplesOK {
				ec.Logger.Warn("encountered error when trying to move migrations from hdb_catalog.schema_migrations to catalog state", fmt.Errorf("invalid result Type %s", runsqlResp.ResultType),
					"\nnote: ignore this if you are not updating your project from config v2 -> config v3")
				ec.Logger.Debug("marking IsStateCopyCompleted as true %w", markStateMigrationCompleted())
				return false, nil
			}
			result := runsqlResp.Result
			if result[1][0] == "0" {
				// hdb_catalog.schema_migrations doesn't exist
				ec.Logger.Debug("hdb_catalog.schema_migrations was not found, skipping state migration")
				ec.Logger.Debug("marking IsStateCopyCompleted as true %w", markStateMigrationCompleted())
				return false, nil
			}
			ec.Logger.Debug("copying cli state from hdb_catalog.schema_migrations to catalog state")
			// COPY STATE
			if err := scripts.CopyState(ec, sourceName); err != nil {
				return false, err
			}
			ec.Logger.Debug("copying cli state from hdb_catalog.schema_migrations to catalog state success")
			return true, nil
		}
		ec.Logger.Debugf("skipping state migration, found IsStateCopyCompleted: %v Migrations: %v", state.IsStateCopyCompleted, state.Migrations)
		return false, nil
	}
	return false, nil
}
