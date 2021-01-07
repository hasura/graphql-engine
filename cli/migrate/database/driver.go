package database

import (
	"crypto/tls"
	"fmt"
	"io"
	"sync"

	nurl "net/url"

	log "github.com/sirupsen/logrus"
)

var (
	ErrLocked = fmt.Errorf("can't acquire lock")
)

const NilVersion int64 = -1

var driversMu sync.RWMutex
var drivers = make(map[string]Driver)

// Driver is the interface every database driver must implement.
//
// How to implement a database driver?
//   1. Implement this interface.
//   2. Optionally, add a function named `WithInstance`.
//      This function should accept an existing DB instance and a Config{} struct
//      and return a driver instance.
//   3. Add a test that calls database/testing.go:Test()
//   4. Add own tests for Open(), WithInstance() (when provided) and Close().
//      All other functions are tested by tests in database/testing.
//      Saves you some time and makes sure all database drivers behave the same way.
//   5. Call Register in init().
//   6. Create a migrate/cli/build_<driver-name>.go file
//   7. Add driver name in 'DATABASE' variable in Makefile
//
// Guidelines:
//   * Don't try to correct user input. Don't assume things.
//     When in doubt, return an error and explain the situation to the user.
//   * All configuration input must come from the URL string in func Open()
//     or the Config{} struct in WithInstance. Don't os.Getenv().
type Driver interface {
	// Open returns a new driver instance configured with parameters
	// coming from the URL string. Migrate will call this function
	// only once per instance.
	Open(url string, isCMD bool, tlsConfig *tls.Config, logger *log.Logger, hasuraOpts *HasuraOpts) (Driver, error)

	// Close closes the underlying database instance managed by the driver.
	// Migrate will call this function only once per instance.
	Close() error

	Scan() error

	// Lock should acquire a database lock so that only one migration process
	// can run at a time. Migrate will call this function before Run is called.
	// If the implementation can't provide this functionality, return nil.
	// Return database.ErrLocked if database is already locked.
	Lock() error

	// Unlock should release the lock. Migrate will call this function after
	// all migrations have been run.
	UnLock() error
	UnLockSeq() error

	// Run applies a migration to the database. migration is guaranteed to be not nil.
	Run(migration io.Reader, fileType, fileName string) error
	// RunSeq applies a migration to the database in a sequential fashion. migration is guaranteed to be not nil.
	RunSeq(migration io.Reader, fileType, fileName string) error

	// Reset Migration Query Args
	ResetQuery()

	// InsertVersion saves version
	// Migrate will call this function before and after each call to Run.
	// version must be >= -1. -1 means NilVersion.
	InsertVersion(version int64) error

	// SetVersion saves version and dirty state.
	// Migrate will call this function before and after each call to Run.
	// version must be >= -1. -1 means NilVersion.
	SetVersion(version int64, dirty bool) error

	// SetVersion saves version and dirty state.
	// Migrate will call this function before and after each call to Run.
	// version must be >= -1. -1 means NilVersion.
	RemoveVersion(version int64) error

	// Version returns the currently active version and if the database is dirty.
	// When no migration has been applied, it must return version -1.
	// Dirty means, a previous migration failed and user interaction is required.
	Version() (version int64, dirty bool, err error)

	// First returns the very first migration version available to the driver.
	// Migrate will call this function multiple times
	First() (migrationVersion *MigrationVersion, ok bool)

	// Last returns the latest version available in database
	Last() (*MigrationVersion, bool)

	// Prev returns the previous version for a given version available to the driver.
	// Migrate will call this function multiple times.
	// If there is no previous version available, it must return os.ErrNotExist.
	Prev(version uint64) (prevVersion *MigrationVersion, ok bool)

	// Next returns the next version for a given version available to the driver.
	// Migrate will call this function multiple times.
	// If there is no next version available, it must return os.ErrNotExist.
	Next(version uint64) (migrationVersion *MigrationVersion, ok bool)

	Read(version uint64) (ok bool)

	PushToList(migration io.Reader, fileType string, list *CustomList) error

	Squash(list *CustomList, ret chan<- interface{})

	MetadataDriver

	GraphQLDriver

	SchemaDriver

	SeedDriver

	SettingsDriver
}

// Open returns a new driver instance.
func Open(url string, isCMD bool, tlsConfig *tls.Config, logger *log.Logger, hasuraOpts *HasuraOpts) (Driver, error) {
	u, err := nurl.Parse(url)
	if err != nil {
		log.Debug(err)
		return nil, err
	}

	driversMu.RLock()
	if u.Scheme == "" {
		return nil, fmt.Errorf("database driver: invalid URL scheme")
	}
	driversMu.RUnlock()

	d, ok := drivers[u.Scheme]
	if !ok {
		return nil, fmt.Errorf("database driver: unknown driver %v", u.Scheme)
	}

	if logger == nil {
		logger = log.New()
	}

	return d.Open(url, isCMD, tlsConfig, logger, hasuraOpts)
}

func Register(name string, driver Driver) {
	driversMu.Lock()
	defer driversMu.Unlock()
	if driver == nil {
		panic("Register driver is nil")
	}
	if _, dup := drivers[name]; dup {
		panic("Register called twice for driver " + name)
	}
	drivers[name] = driver
}
