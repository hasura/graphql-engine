package database

import (
	"sort"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

// Migrations wraps Migration and has an internal index
// to keep track of Migration order in database.
type Migrations struct {
	index migrationVersions
}

func NewMigrations() *Migrations {
	return &Migrations{
		index: make(migrationVersions, 0),
	}
}

func (m *Migrations) Append(migrationVersion MigrationVersion) {
	if m.findPos(migrationVersion.Version) > 0 {
		return
	}
	m.index = append(m.index, migrationVersion)

	sort.Slice(m.index, func(i, j int) bool {
		return m.index[i].Version < m.index[j].Version
	})

}

func (m *Migrations) First() (migrationVersion *MigrationVersion, ok bool) {
	if len(m.index) == 0 {
		return nil, false
	}
	return &m.index[0], true
}

func (m *Migrations) Last() (*MigrationVersion, bool) {
	if len(m.index) == 0 {
		return nil, false
	}

	return &m.index[len(m.index)-1], true
}

func (m *Migrations) Prev(version uint64) (prevVersion *MigrationVersion, ok bool) {
	pos := m.findPos(version)
	if pos >= 1 && len(m.index) > pos-1 {
		return &m.index[pos-1], true
	}
	return nil, false
}

func (m *Migrations) Next(version uint64) (migrationVersion *MigrationVersion, ok bool) {
	pos := m.findPos(version)
	if pos >= 0 && len(m.index) > pos+1 {
		return &m.index[pos+1], true
	}
	return nil, false
}

func (m *Migrations) Read(version uint64) (ok bool) {
	pos := m.findPos(version)
	return pos >= 0
}

func (m *Migrations) findPos(version uint64) int {
	if len(m.index) > 0 {
		ix := m.index.Search(version)
		if ix < len(m.index) && m.index[ix].Version == version {
			return ix
		}
	}
	return -1
}

type MigrationVersion struct {
	Version uint64
	Dirty   bool
}

type migrationVersions []MigrationVersion

func (s migrationVersions) Len() int {
	return len(s)
}

func (s migrationVersions) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s migrationVersions) Less(i, j int) bool {
	return s[i].Version < s[j].Version
}

func (s migrationVersions) Search(x uint64) int {
	return sort.Search(len(s), func(i int) bool { return s[i].Version >= x })
}

type HasuraOpts struct {
	HasMetadataV3 bool
	SourceName    string
	SourceKind    hasura.SourceKind

	Client *hasura.Client

	PGSourceOps         hasura.PGSourceOps
	MSSQLSourceOps      hasura.MSSQLSourceOps
	CitusSourceOps      hasura.CitusSourceOps
	BigQuerySourceOps   hasura.BigQuerySourceOps
	MetadataOps         hasura.CommonMetadataOperations
	V2MetadataOps       hasura.V2CommonMetadataOperations
	GenericQueryRequest hasura.GenericSend
	PGDumpClient        hasura.PGDump

	MigrationsStateStore statestore.MigrationsStateStore
	SettingsStateStore   statestore.SettingsStateStore
}
