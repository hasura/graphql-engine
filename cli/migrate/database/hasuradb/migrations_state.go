package hasuradb

// Abstraction for the storage layer for migration state
type MigrationsStateStore interface {
	InsertVersion(version int64) error
	RemoveVersion(version int64) error
	SetVersion(version int64, dirty bool) error
	GetVersions() error

	// This method is expected to initialize the datastore
	// and validate it
	PrepareMigrationsStateStore() error
}
