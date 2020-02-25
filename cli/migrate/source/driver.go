package source

import (
	"fmt"
	"io"
	nurl "net/url"
	"sync"

	log "github.com/sirupsen/logrus"
)

var driversMu sync.RWMutex
var drivers = make(map[string]Driver)

// Driver is the interface every source driver must implement.
//
// How to implement a source driver?
//   1. Implement this interface.
//   2. Optionally, add a function named `WithInstance`.
//      This function should accept an existing source instance and a Config{} struct
//      and return a driver instance.
//   3. Add a test that calls source/testing.go:Test()
//   4. Add own tests for Open(), WithInstance() (when provided) and Close().
//      All other functions are tested by tests in source/testing.
//      Saves you some time and makes sure all source drivers behave the same way.
//   5. Call Register in init().
//
// Guidelines:
//   * All configuration input must come from the URL string in func Open()
//     or the Config{} struct in WithInstance. Don't os.Getenv().
//   * Drivers are supposed to be read only.
//   * Ideally don't load any contents (into memory) in Open or WithInstance.
type Driver interface {
	// Open returns a new driver instance configured with parameters
	// coming from the URL string. Migrate will call this function
	// only once per instance.
	Open(url string, logger *log.Logger) (Driver, error)

	// Close closes the underlying source instance managed by the driver.
	// Migrate will call this function only once per instance.
	Close() error

	// Scan scans the local migration files
	Scan() error

	// Default Parser to be used for scanning the file system
	DefaultParser(Parser)

	// First returns the very first migration version available to the driver.
	// Migrate will call this function multiple times.
	// If there is no version available, it must return os.ErrNotExist.
	First() (version uint64, err error)

	// GetLocalVersion returns the latest version available in migrations folder
	GetLocalVersion() (version uint64, err error)

	// Get all unapplied migrations present in local directory
	GetUnappliedMigrations(version uint64) (versions []uint64)

	// Prev returns the previous version for a given version available to the driver.
	// Migrate will call this function multiple times.
	// If there is no previous version available, it must return os.ErrNotExist.
	Prev(version uint64) (prevVersion uint64, err error)

	// Next returns the next version for a given version available to the driver.
	// Migrate will call this function multiple times.
	// If there is no next version available, it must return os.ErrNotExist.
	Next(version uint64) (nextVersion uint64, err error)

	GetDirections(version uint64) map[Direction]bool

	// ReadUp returns the UP migration body and an identifier that helps
	// finding this migration in the source for a given version.
	// If there is no up migration available for this version,
	// it must return os.ErrNotExist.
	// Do not start reading, just return the ReadCloser!
	ReadUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error)

	// ReadUp returns the UP migration body and an identifier that helps
	// finding this migration in the source for a given version.
	// If there is no up migration available for this version,
	// it must return os.ErrNotExist.
	// Do not start reading, just return the ReadCloser!
	ReadMetaUp(version uint64) (r io.ReadCloser, identifier string, fileName string, err error)

	// ReadDown returns the DOWN migration body and an identifier that helps
	// finding this migration in the source for a given version.
	// If there is no down migration available for this version,
	// it must return os.ErrNotExist.
	// Do not start reading, just return the ReadCloser!
	ReadDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error)

	// ReadDown returns the DOWN migration body and an identifier that helps
	// finding this migration in the source for a given version.
	// If there is no down migration available for this version,
	// it must return os.ErrNotExist.
	// Do not start reading, just return the ReadCloser!
	ReadMetaDown(version uint64) (r io.ReadCloser, identifier string, fileName string, err error)

	// ReadName returns an name that helps
	// finding this migration in the source for a given version
	ReadName(version uint64) (name string)

	// WriteMetadaa writes the files in the metadata folder
	WriteMetadata(files map[string][]byte) error
}

// Open returns a new driver instance.
func Open(url string, logger *log.Logger) (Driver, error) {
	u, err := nurl.Parse(url)
	if err != nil {
		return nil, err
	}

	if u.Scheme == "" {
		return nil, fmt.Errorf("source driver: invalid URL scheme")
	}

	driversMu.RLock()
	d, ok := drivers[u.Scheme]
	driversMu.RUnlock()
	if !ok {
		return nil, fmt.Errorf("source driver: unknown driver %v (forgotten import?)", u.Scheme)
	}

	if logger == nil {
		logger = log.New()
	}

	return d.Open(url, logger)
}

// Register globally registers a driver.
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
