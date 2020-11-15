package hasuradb

// until version 1.4 migration state was stored a special table
// this struct will implement the methods required
type MigrationStateWithSQL struct{}

func (m *MigrationStateWithSQL) InsertVersion(version int64) error {
	return nil
}

func (m *MigrationStateWithSQL) RemoveVersion(version int64) error {
	return nil
}

func (m *MigrationStateWithSQL) Prepare() error {
	return nil
}

// from v1.4 clients are expceted to make use of the catalog API
// rather than assuming a SQL backend for metadata storage
type MigrationsStateWithCatalogStateAPI struct {
	hasuradb HasuraDB
	State    *MigrationsState
}

func (m *MigrationsStateWithCatalogStateAPI) InsertVersion(version int64) error {
	return nil
}

func (m *MigrationsStateWithCatalogStateAPI) RemoveVersion(version int64) error {
	return nil
}

func (m *MigrationsStateWithCatalogStateAPI) Prepare() error {
	return nil
}
