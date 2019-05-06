// Package migrate implements migrations on Hasura GraphQL Engine.
//
// This package is borrowed from https://github.com/golang-migrate/migrate with
// additions for Hasura specific yaml file support and a improved Rails-like
// migration pattern.
package migrate

import (
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/hasura/graphql-engine/cli/migrate/source"

	log "github.com/sirupsen/logrus"
)

// DefaultPrefetchMigrations sets the number of migrations to pre-read
// from the source. This is helpful if the source is remote, but has little
// effect for a local source (i.e. file system).
// Please note that this setting has a major impact on the memory usage,
// since each pre-read migration is buffered in memory. See DefaultBufferSize.
var DefaultPrefetchMigrations = uint64(10)

// DefaultLockTimeout sets the max time a database driver has to acquire a lock.
var DefaultLockTimeout = 15 * time.Second

var (
	ErrNoChange         = fmt.Errorf("no change")
	ErrNilVersion       = fmt.Errorf("no migration")
	ErrLocked           = fmt.Errorf("database locked")
	ErrNoMigrationFiles = fmt.Errorf("no migration files found")
	ErrLockTimeout      = fmt.Errorf("timeout: can't acquire database lock")
	ErrApplied          = fmt.Errorf("Version already applied in database")
	ErrNotApplied       = fmt.Errorf("Migration not applied in database")
	ErrNoMigrationMode  = fmt.Errorf("Migration mode is disabled")
	ErrMigrationMode    = fmt.Errorf("Migration mode is enabled")
)

// ErrShortLimit is an error returned when not enough migrations
// can be returned by a source for a given limit.
type ErrShortLimit struct {
	Short uint64
}

// Error implements the error interface.
func (e ErrShortLimit) Error() string {
	return fmt.Sprintf("limit %v short", e.Short)
}

type ErrDirty struct {
	Version int64
}

func (e ErrDirty) Error() string {
	return fmt.Sprintf("Dirty database version %v. Fix and force version.", e.Version)
}

type Migrate struct {
	sourceName string
	sourceURL  string
	sourceDrv  source.Driver

	databaseName string
	databaseURL  string
	databaseDrv  database.Driver

	// Logger is the global logger object to print logs.
	Logger *log.Logger

	// GracefulStop accepts `true` and will stop executing migrations
	// as soon as possible at a safe break point, so that the database
	// is not corrupted.
	GracefulStop   chan bool
	isGracefulStop bool

	isLockedMu *sync.Mutex
	isLocked   bool

	// PrefetchMigrations defaults to DefaultPrefetchMigrations,
	// but can be set per Migrate instance.
	PrefetchMigrations uint64

	// LockTimeout defaults to DefaultLockTimeout,
	// but can be set per Migrate instance.
	LockTimeout time.Duration

	//CMD
	isCMD bool

	status *Status

	SkipExecution bool
}

// New returns a new Migrate instance from a source URL and a database URL.
// The URL scheme is defined by each driver.
func New(sourceUrl string, databaseUrl string, cmd bool, logger *log.Logger) (*Migrate, error) {
	m := newCommon(cmd)

	sourceName, err := schemeFromUrl(sourceUrl)
	if err != nil {
		log.Debug(err)
		return nil, err
	}
	m.sourceName = sourceName
	m.sourceURL = sourceUrl

	databaseName, err := schemeFromUrl(databaseUrl)
	if err != nil {
		log.Debug(err)
		return nil, err
	}
	m.databaseName = databaseName
	m.databaseURL = databaseUrl

	if logger == nil {
		logger = log.New()
	}

	sourceDrv, err := source.Open(sourceUrl, logger)
	if err != nil {
		log.Debug(err)
		return nil, err
	}
	m.sourceDrv = sourceDrv

	databaseDrv, err := database.Open(databaseUrl, cmd, logger)
	if err != nil {
		log.Debug(err)
		return nil, err
	}
	m.databaseDrv = databaseDrv

	m.status = NewStatus()

	return m, nil
}

func newCommon(cmd bool) *Migrate {
	return &Migrate{
		GracefulStop:       make(chan bool, 1),
		PrefetchMigrations: DefaultPrefetchMigrations,
		isLockedMu:         &sync.Mutex{},
		LockTimeout:        DefaultLockTimeout,
		isCMD:              cmd,
	}
}

func (m *Migrate) ReScan() error {
	sourceDrv, err := source.Open(m.sourceURL, m.Logger)
	if err != nil {
		m.Logger.Debug(err)
		return err
	}
	m.sourceDrv = sourceDrv

	databaseDrv, err := database.Open(m.databaseURL, m.isCMD, m.Logger)
	if err != nil {
		m.Logger.Debug(err)
		return err
	}
	m.databaseDrv = databaseDrv

	err = m.calculateStatus()
	if err != nil {
		return err
	}
	return nil
}

// Close closes the source and the database.
func (m *Migrate) Close() (source error) {
	sourceSrvClose := make(chan error)

	go func() {
		sourceSrvClose <- m.sourceDrv.Close()
	}()

	return <-sourceSrvClose
}

func (m *Migrate) calculateStatus() (err error) {
	m.status = NewStatus()
	err = m.readStatusFromSource()
	if err != nil {
		return err
	}

	return m.readStatusFromDatabase()
}

func (m *Migrate) readStatusFromSource() (err error) {
	firstVersion, err := m.sourceDrv.First()
	if err != nil {
		if _, ok := err.(*os.PathError); ok {
			return nil
		}
		return err
	}
	m.status.Append(m.newMigrationStatus(firstVersion, "source"))
	from := int64(firstVersion)

	lastVersion, err := m.sourceDrv.GetLocalVersion()
	if err != nil {
		return err
	}
	m.status.Append(m.newMigrationStatus(lastVersion, "source"))
	to := int64(lastVersion)

	for from < to {
		next, err := m.sourceDrv.Next(suint64(from))
		if err != nil {
			return err
		}
		m.status.Append(m.newMigrationStatus(next, "source"))
		from = int64(next)
	}

	return nil
}

func (m *Migrate) readStatusFromDatabase() (err error) {
	firstVersion, ok := m.databaseDrv.First()
	if !ok {
		return nil
	}
	m.status.Append(m.newMigrationStatus(firstVersion, "database"))
	from := int64(firstVersion)

	lastVersion, ok := m.databaseDrv.Last()
	if !ok {
		return nil
	}
	m.status.Append(m.newMigrationStatus(lastVersion, "database"))
	to := int64(lastVersion)

	for from < to {
		next, ok := m.databaseDrv.Next(suint64(from))
		if !ok {
			return nil
		}
		m.status.Append(m.newMigrationStatus(next, "database"))
		from = int64(next)
	}
	return err
}

func (m *Migrate) newMigrationStatus(version uint64, driverType string) *MigrationStatus {
	var migrStatus *MigrationStatus
	migrStatus, ok := m.status.Read(version)
	if !ok {
		migrStatus = &MigrationStatus{
			Version: version,
		}
	}

	switch driverType {
	case "source":
		migrStatus.IsPresent = true
	case "database":
		migrStatus.IsApplied = true
	default:
		return nil
	}
	return migrStatus
}

func (m *Migrate) GetStatus() (*Status, error) {
	err := m.calculateStatus()
	if err != nil {
		return nil, err
	}
	return m.status, nil
}

func (m *Migrate) GetSetting(name string) (string, error) {
	val, err := m.databaseDrv.GetSetting(name)
	if err != nil {
		return "", err
	}
	return val, nil
}

func (m *Migrate) UpdateSetting(name string, value string) error {
	return m.databaseDrv.UpdateSetting(name, value)
}

func (m *Migrate) Version() (version uint64, dirty bool, err error) {
	v, d, err := m.databaseDrv.Version()
	if err != nil {
		return 0, false, err
	}

	if v == database.NilVersion {
		return 0, false, ErrNilVersion
	}

	return suint64(v), d, nil
}

func (m *Migrate) GetUnappliedMigrations(version uint64) []uint64 {
	return m.sourceDrv.GetUnappliedMigrations(version)
}

func (m *Migrate) ExportMetadata() (interface{}, error) {
	return m.databaseDrv.ExportMetadata()
}

func (m *Migrate) ResetMetadata() error {
	return m.databaseDrv.ResetMetadata()
}

// ReloadMetadata - Reload metadata on the database
func (m *Migrate) ReloadMetadata() error {
	return m.databaseDrv.ReloadMetadata()
}

func (m *Migrate) ApplyMetadata(data interface{}) error {
	return m.databaseDrv.ApplyMetadata(data)
}

func (m *Migrate) ExportSchemaDump(schemName []string) ([]byte, error) {
	return m.databaseDrv.ExportSchemaDump(schemName)
}

func (m *Migrate) Query(data []interface{}) error {
	mode, err := m.databaseDrv.GetSetting("migration_mode")
	if err != nil {
		return err
	}

	if mode == "true" {
		return ErrMigrationMode
	}

	return m.databaseDrv.Query(data)
}

// Migrate looks at the currently active migration version,
// then migrates either up or down to the specified version.
func (m *Migrate) Migrate(version uint64, direction string) error {
	mode, err := m.databaseDrv.GetSetting("migration_mode")
	if err != nil {
		return err
	}

	if mode != "true" {
		return ErrNoMigrationMode
	}

	if err := m.lock(); err != nil {
		return err
	}

	ret := make(chan interface{}, m.PrefetchMigrations)
	go m.read(version, direction, ret)

	return m.unlockErr(m.runMigrations(ret))
}

// Steps looks at the currently active migration version.
// It will migrate up if n > 0, and down if n < 0.
func (m *Migrate) Steps(n int64) error {
	mode, err := m.databaseDrv.GetSetting("migration_mode")
	if err != nil {
		return err
	}

	if mode != "true" {
		return ErrNoMigrationMode
	}

	if n == 0 {
		return ErrNoChange
	}

	if err := m.lock(); err != nil {
		return err
	}

	ret := make(chan interface{}, m.PrefetchMigrations)

	if n > 0 {
		go m.readUp(n, ret)
	} else {
		go m.readDown(-n, ret)
	}

	return m.unlockErr(m.runMigrations(ret))
}

// Up looks at the currently active migration version
// and will migrate all the way up (applying all up migrations).
func (m *Migrate) Up() error {
	mode, err := m.databaseDrv.GetSetting("migration_mode")
	if err != nil {
		return err
	}

	if mode != "true" {
		return ErrNoMigrationMode
	}

	if err := m.lock(); err != nil {
		return err
	}

	curVersion, dirty, err := m.databaseDrv.Version()
	if err != nil {
		return err
	}

	if dirty {
		return ErrDirty{curVersion}
	}

	ret := make(chan interface{}, m.PrefetchMigrations)

	go m.readUp(-1, ret)

	return m.unlockErr(m.runMigrations(ret))
}

// Down looks at the currently active migration version
// and will migrate all the way down (applying all down migrations).
func (m *Migrate) Down() error {
	mode, err := m.databaseDrv.GetSetting("migration_mode")
	if err != nil {
		return err
	}

	if mode != "true" {
		return ErrNoMigrationMode
	}

	if err := m.lock(); err != nil {
		return err
	}

	curVersion, dirty, err := m.databaseDrv.Version()
	if err != nil {
		return err
	}

	if dirty {
		return ErrDirty{curVersion}
	}

	ret := make(chan interface{}, m.PrefetchMigrations)
	go m.readDown(-1, ret)

	return m.unlockErr(m.runMigrations(ret))
}

// Reset resets public schema and hasuradb metadata
func (m *Migrate) Reset() (err error) {
	err = m.databaseDrv.Reset()
	return
}

// read reads either up or down migrations from source `from` to `to`.
// Each migration is then written to the ret channel.
// If an error occurs during reading, that error is written to the ret channel, too.
// Once read is done reading it will close the ret channel.
func (m *Migrate) read(version uint64, direction string, ret chan<- interface{}) {
	defer close(ret)

	if direction == "up" {
		if m.stop() {
			return
		}

		// Check if this version present in DB
		ok := m.databaseDrv.Read(version)
		if ok {
			ret <- ErrApplied
			return
		}

		// Check if next version exiss (yaml or sql)
		if err := m.versionUpExists(version); err != nil {
			ret <- err
			return
		}

		migr, err := m.newMigration(version, int64(version))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()

		migr, err = m.metanewMigration(version, int64(version))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()

	} else {
		// it's going down
		if m.stop() {
			return
		}

		// Check if this version present in DB
		ok := m.databaseDrv.Read(version)
		if !ok {
			ret <- ErrNotApplied
			return
		}

		if err := m.versionDownExists(version); err != nil {
			ret <- err
			return
		}

		prev, err := m.sourceDrv.Prev(version)
		if os.IsNotExist(err) {
			// apply nil migration
			migr, err := m.newMigration(version, -1)
			if err != nil {
				ret <- err
				return
			}
			ret <- migr
			go migr.Buffer()

			migr, err = m.metanewMigration(version, -1)
			if err != nil {
				ret <- err
				return
			}
			ret <- migr
			go migr.Buffer()
			return
		} else if err != nil {
			ret <- err
			return
		}

		migr, err := m.newMigration(version, int64(prev))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()

		migr, err = m.metanewMigration(version, int64(prev))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()
	}
}

// readUp reads up migrations from `from` limitted by `limit`.
// limit can be -1, implying no limit and reading until there are no more migrations.
// Each migration is then written to the ret channel.
// If an error occurs during reading, that error is written to the ret channel, too.
// Once readUp is done reading it will close the ret channel.
func (m *Migrate) readUp(limit int64, ret chan<- interface{}) {
	defer close(ret)

	if limit == 0 {
		ret <- ErrNoChange
		return
	}

	count := int64(0)
	from := int64(-1)
	for count < limit || limit == -1 {
		if m.stop() {
			return
		}

		if from == -1 {
			firstVersion, err := m.sourceDrv.First()
			if err != nil {
				ret <- err
				return
			}

			// Check if this version present in DB
			ok := m.databaseDrv.Read(firstVersion)
			if ok {
				from = int64(firstVersion)
				continue
			}

			// Check if firstVersion files exists (yaml or sql)
			if err = m.versionUpExists(firstVersion); err != nil {
				ret <- err
				return
			}

			migr, err := m.newMigration(firstVersion, int64(firstVersion))
			if err != nil {
				ret <- err
				return
			}
			ret <- migr
			go migr.Buffer()

			migr, err = m.metanewMigration(firstVersion, int64(firstVersion))
			if err != nil {
				ret <- err
				return
			}
			ret <- migr
			go migr.Buffer()
			from = int64(firstVersion)
			count++
			continue
		}

		// apply next migration
		next, err := m.sourceDrv.Next(suint64(from))
		if os.IsNotExist(err) {
			// no limit, but no migrations applied?
			if limit == -1 && count == 0 {
				ret <- ErrNoChange
				return
			}

			// no limit, reached end
			if limit == -1 {
				return
			}

			// reached end, and didn't apply any migrations
			if limit > 0 && count == 0 {
				ret <- ErrNoChange
				return
			}

			// applied less migrations than limit?
			if count < limit {
				ret <- ErrShortLimit{suint64(limit - count)}
				return
			}
		}

		if err != nil {
			ret <- err
			return
		}

		// Check if this version present in DB
		ok := m.databaseDrv.Read(next)
		if ok {
			from = int64(next)
			continue
		}

		// Check if next files exists (yaml or sql)
		if err = m.versionUpExists(next); err != nil {
			ret <- err
			return
		}

		migr, err := m.newMigration(next, int64(next))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()

		migr, err = m.metanewMigration(next, int64(next))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()
		from = int64(next)
		count++
	}
}

// readDown reads down migrations from `from` limitted by `limit`.
// limit can be -1, implying no limit and reading until there are no more migrations.
// Each migration is then written to the ret channel.
// If an error occurs during reading, that error is written to the ret channel, too.
// Once readDown is done reading it will close the ret channel.
func (m *Migrate) readDown(limit int64, ret chan<- interface{}) {
	defer close(ret)

	if limit == 0 {
		ret <- ErrNoChange
		return
	}

	from, _, err := m.databaseDrv.Version()
	if err != nil {
		ret <- err
		return
	}

	// no change if already at nil version
	if from == -1 && limit == -1 {
		ret <- ErrNoChange
		return
	}

	// can't go over limit if already at nil version
	if from == -1 && limit > 0 {
		ret <- ErrNoChange
		return
	}

	count := int64(0)
	for count < limit || limit == -1 {
		if m.stop() {
			return
		}

		err = m.versionDownExists(suint64(from))
		if err != nil {
			ret <- err
			return
		}

		prev, ok := m.databaseDrv.Prev(suint64(from))
		if !ok {
			// no limit or haven't reached limit, apply "first" migration
			if limit == -1 || limit-count > 0 {
				migr, err := m.metanewMigration(suint64(from), -1)
				if err != nil {
					ret <- err
					return
				}
				ret <- migr
				go migr.Buffer()

				migr, err = m.newMigration(suint64(from), -1)
				if err != nil {
					ret <- err
					return
				}
				ret <- migr
				go migr.Buffer()
				count++
			}

			if count < limit {
				ret <- ErrShortLimit{suint64(limit - count)}
			}
			return
		}

		migr, err := m.metanewMigration(suint64(from), int64(prev))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()

		migr, err = m.newMigration(suint64(from), int64(prev))
		if err != nil {
			ret <- err
			return
		}

		ret <- migr
		go migr.Buffer()
		from = int64(prev)
		count++
	}
}

// runMigrations reads *Migration and error from a channel. Any other type
// sent on this channel will result in a panic. Each migration is then
// proxied to the database driver and run against the database.
// Before running a newly received migration it will check if it's supposed
// to stop execution because it might have received a stop signal on the
// GracefulStop channel.
func (m *Migrate) runMigrations(ret <-chan interface{}) error {
	var lastInsertVersion int64
	for r := range ret {
		if m.stop() {
			return nil
		}

		switch r.(type) {
		case error:
			// Clear Migration query
			m.databaseDrv.ResetQuery()
			return r.(error)
		case *Migration:
			migr := r.(*Migration)
			if migr.Body != nil {
				if !m.SkipExecution {
					if err := m.databaseDrv.Run(migr.BufferedBody, migr.FileType, migr.FileName); err != nil {
						return err
					}
				}

				version := int64(migr.Version)
				if version == migr.TargetVersion {
					if version != lastInsertVersion {
						// Insert Version number into the table
						if err := m.databaseDrv.InsertVersion(version); err != nil {
							return err
						}
						lastInsertVersion = version
					}
				} else {
					// Delete Version number from the table
					if err := m.databaseDrv.RemoveVersion(version); err != nil {
						return err
					}
				}
			}
		}
	}
	return nil
}

// versionUpExists checks the source if either the up or down migration for
// the specified migration version exists.
func (m *Migrate) versionUpExists(version uint64) error {
	// try up migration first
	directions := m.sourceDrv.GetDirections(version)
	if !directions[source.Up] && !directions[source.MetaUp] {
		return fmt.Errorf("%d up migration not found", version)
	}

	if directions[source.Up] {
		up, _, _, err := m.sourceDrv.ReadUp(version)
		if err == nil {
			defer up.Close()
		}

		if os.IsExist(err) {
			return nil
		} else if !os.IsNotExist(err) {
			return err
		}
	}

	if directions[source.MetaUp] {
		up, _, _, err := m.sourceDrv.ReadMetaUp(version)
		if err == nil {
			defer up.Close()
		}

		if os.IsExist(err) {
			return nil
		} else if !os.IsNotExist(err) {
			return err
		}
	}

	return os.ErrNotExist
}

// versionDownExists checks the source if either the up or down migration for
// the specified migration version exists.
func (m *Migrate) versionDownExists(version uint64) error {
	// try up migration first
	directions := m.sourceDrv.GetDirections(version)
	if !directions[source.Down] && !directions[source.MetaDown] {
		return fmt.Errorf("%d down migration not found", version)
	}

	if directions[source.Down] {
		up, _, _, err := m.sourceDrv.ReadDown(version)
		if err == nil {
			defer up.Close()
		}

		if os.IsExist(err) {
			return nil
		} else if !os.IsNotExist(err) {
			return err
		}
	}

	if directions[source.MetaDown] {
		up, _, _, err := m.sourceDrv.ReadMetaDown(version)
		if err == nil {
			defer up.Close()
		}

		if os.IsExist(err) {
			return nil
		} else if !os.IsNotExist(err) {
			return err
		}
	}

	return os.ErrNotExist
}

// newMigration is a helper func that returns a *Migration for the
// specified version and targetVersion (sql).
func (m *Migrate) newMigration(version uint64, targetVersion int64) (*Migration, error) {
	var migr *Migration

	if targetVersion >= int64(version) {
		r, identifier, fileName, err := m.sourceDrv.ReadUp(version)
		if os.IsNotExist(err) {
			// create "empty" migration
			migr, err = NewMigration(nil, "", version, targetVersion, "sql", "")
			if err != nil {
				return nil, err
			}

		} else if err != nil {
			return nil, err

		} else {
			// create migration from up source
			migr, err = NewMigration(r, identifier, version, targetVersion, "sql", fileName)
			if err != nil {
				return nil, err
			}
		}

	} else {
		r, identifier, fileName, err := m.sourceDrv.ReadDown(version)
		if os.IsNotExist(err) {
			// create "empty" migration
			migr, err = NewMigration(nil, "", version, targetVersion, "sql", "")
			if err != nil {
				return nil, err
			}

		} else if err != nil {
			return nil, err

		} else {
			// create migration from down source
			migr, err = NewMigration(r, identifier, version, targetVersion, "sql", fileName)
			if err != nil {
				return nil, err
			}
		}
	}

	if m.PrefetchMigrations > 0 && migr.Body != nil {
		//m.logVerbosePrintf("Start buffering %v\n", migr.LogString())
	} else {
		//m.logVerbosePrintf("Scheduled %v\n", migr.LogString())
	}

	return migr, nil
}

// metanewMigration is a helper func that returns a *Migration for the
// specified version and targetVersion (yaml).
func (m *Migrate) metanewMigration(version uint64, targetVersion int64) (*Migration, error) {
	var migr *Migration

	if targetVersion >= int64(version) {
		r, identifier, fileName, err := m.sourceDrv.ReadMetaUp(version)
		if os.IsNotExist(err) {
			// create "empty" migration
			migr, err = NewMigration(nil, "", version, targetVersion, "meta", "")
			if err != nil {
				return nil, err
			}

		} else if err != nil {
			return nil, err

		} else {
			// create migration from up source
			migr, err = NewMigration(r, identifier, version, targetVersion, "meta", fileName)
			if err != nil {
				return nil, err
			}
		}

	} else {
		r, identifier, fileName, err := m.sourceDrv.ReadMetaDown(version)
		if os.IsNotExist(err) {
			// create "empty" migration
			migr, err = NewMigration(nil, "", version, targetVersion, "meta", "")
			if err != nil {
				return nil, err
			}

		} else if err != nil {
			return nil, err

		} else {
			// create migration from down source
			migr, err = NewMigration(r, identifier, version, targetVersion, "meta", fileName)
			if err != nil {
				return nil, err
			}
		}
	}

	if m.PrefetchMigrations > 0 && migr.Body != nil {
		//m.logVerbosePrintf("Start buffering %v\n", migr.LogString())
	} else {
		//m.logVerbosePrintf("Scheduled %v\n", migr.LogString())
	}

	return migr, nil
}

// stop returns true if no more migrations should be run against the database
// because a stop signal was received on the GracefulStop channel.
// Calls are cheap and this function is not blocking.
func (m *Migrate) stop() bool {
	if m.isGracefulStop {
		return true
	}

	select {
	case <-m.GracefulStop:
		m.isGracefulStop = true
		return true

	default:
		return false
	}
}

// lock is a thread safe helper function to lock the database.
// It should be called as late as possible when running migrations.
func (m *Migrate) lock() error {
	m.isLockedMu.Lock()
	defer m.isLockedMu.Unlock()

	if m.isLocked {
		return ErrLocked
	}

	// create done channel, used in the timeout goroutine
	done := make(chan bool, 1)
	defer func() {
		done <- true
	}()

	// use errchan to signal error back to this context
	errchan := make(chan error, 2)

	// start timeout goroutine
	timeout := time.After(m.LockTimeout)
	go func() {
		for {
			select {
			case <-done:
				return
			case <-timeout:
				errchan <- ErrLockTimeout
				return
			}
		}
	}()

	// now try to acquire the lock
	go func() {
		if err := m.databaseDrv.Lock(); err != nil {
			errchan <- err
		} else {
			errchan <- nil
		}
		return
	}()

	// wait until we either recieve ErrLockTimeout or error from Lock operation
	err := <-errchan
	if err == nil {
		m.isLocked = true
	}
	return err
}

// unlock is a thread safe helper function to unlock the database.
// It should be called as early as possible when no more migrations are
// expected to be executed.
func (m *Migrate) unlock() error {
	m.isLockedMu.Lock()
	defer m.isLockedMu.Unlock()

	if err := m.databaseDrv.UnLock(); err != nil {
		return err
	}

	m.isLocked = false
	return nil
}

// unlockErr calls unlock and returns a combined error
// if a prevErr is not nil.
func (m *Migrate) unlockErr(prevErr error) error {
	if err := m.unlock(); err != nil {
		return NewMultiError(prevErr, err)
	}
	return prevErr
}
