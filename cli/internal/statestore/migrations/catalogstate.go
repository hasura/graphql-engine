package migrations

import (
	"fmt"
	"strconv"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"
)

// from v1.4 clients are expected to make use of the catalog API
// rather than assuming a SQL backend for metadata storage
type CatalogStateStore struct {
	c *statestore.CLICatalogState
}

func (m *CatalogStateStore) getCLIState() (*statestore.CLIState, error) {
	var op errors.Op = "migrations.CatalogStateStore.getCLIState"
	clistate, err := m.c.Get()
	if err != nil {
		return nil, errors.E(op, err)
	}
	clistate.Init()
	return clistate, nil
}

func (m *CatalogStateStore) setCLIState(state statestore.CLIState) error {
	var op errors.Op = "migrations.CatalogStateStore.setCLIState"
	_, err := m.c.Set(state)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func NewCatalogStateStore(c *statestore.CLICatalogState) *CatalogStateStore {
	return &CatalogStateStore{c}
}

func (m *CatalogStateStore) InsertVersion(database string, version int64) error {
	var op errors.Op = "migrations.CatalogStateStore.InsertVersion"
	// get setting
	state, err := m.getCLIState()
	if err != nil {
		return errors.E(op, err)
	}
	versionString := fmt.Sprintf("%d", version)
	state.SetMigration(database, versionString, false)
	if err := m.setCLIState(*state); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *CatalogStateStore) SetVersion(database string, version int64, dirty bool) error {
	var op errors.Op = "migrations.CatalogStateStore.SetVersion"
	// get setting
	state, err := m.getCLIState()
	if err != nil {
		return errors.E(op, err)
	}
	versionString := fmt.Sprintf("%d", version)
	state.SetMigration(database, versionString, dirty)
	if err := m.setCLIState(*state); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *CatalogStateStore) RemoveVersion(database string, version int64) error {
	var op errors.Op = "migrations.CatalogStateStore.RemoveVersion"
	versionString := fmt.Sprintf("%d", version)
	state, err := m.getCLIState()
	if err != nil {
		return errors.E(op, err)
	}
	state.UnsetMigration(database, versionString)
	if err := m.setCLIState(*state); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *CatalogStateStore) PrepareMigrationsStateStore(_ string) error {
	return nil
}

func (m *CatalogStateStore) GetVersions(database string) (map[uint64]bool, error) {
	var op errors.Op = "migrations.CatalogStateStore.GetVersions"
	state, err := m.getCLIState()
	if err != nil {
		return nil, errors.E(op, err)
	}
	var versions = map[uint64]bool{}
	for version, dirty := range state.GetMigrationsByDatabase(database) {
		parsedVersion, err := strconv.ParseUint(version, 10, 64)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("parsing migration version: %w", err))
		}
		versions[parsedVersion] = dirty
	}
	return versions, nil
}

func (m *CatalogStateStore) SetVersions(database string, versions []statestore.Version) error {
	var op errors.Op = "migrations.CatalogStateStore.SetVersions"
	state, err := m.getCLIState()
	if err != nil {
		return errors.E(op, err)
	}
	for _, v := range versions {
		versionString := fmt.Sprintf("%d", v.Version)
		state.SetMigration(database, versionString, v.Dirty)
	}
	if err := m.setCLIState(*state); err != nil {
		return errors.E(op, err)
	}
	return nil
}
