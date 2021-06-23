package migrations

import (
	"fmt"
	"strconv"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"

	"github.com/pkg/errors"
)

// from v1.4 clients are expected to make use of the catalog API
// rather than assuming a SQL backend for metadata storage
type CatalogStateStore struct {
	c *statestore.CLICatalogState
}

func (m *CatalogStateStore) getCLIState() (*statestore.CLIState, error) {
	clistate, err := m.c.Get()
	if err != nil {
		return nil, err
	}
	clistate.Init()
	return clistate, nil
}

func (m *CatalogStateStore) setCLIState(state statestore.CLIState) error {
	_, err := m.c.Set(state)
	if err != nil {
		return err
	}
	return nil
}

func NewCatalogStateStore(c *statestore.CLICatalogState) *CatalogStateStore {
	return &CatalogStateStore{c}
}

func (m *CatalogStateStore) InsertVersion(database string, version int64) error {
	// get setting
	state, err := m.getCLIState()
	if err != nil {
		return err
	}
	versionString := fmt.Sprintf("%d", version)
	state.SetMigration(database, versionString, false)
	return m.setCLIState(*state)
}

func (m *CatalogStateStore) SetVersion(database string, version int64, dirty bool) error {
	// get setting
	state, err := m.getCLIState()
	if err != nil {
		return err
	}
	versionString := fmt.Sprintf("%d", version)
	state.SetMigration(database, versionString, dirty)
	return m.setCLIState(*state)
}

func (m *CatalogStateStore) RemoveVersion(database string, version int64) error {
	versionString := fmt.Sprintf("%d", version)
	state, err := m.getCLIState()
	if err != nil {
		return err
	}
	state.UnsetMigration(database, versionString)
	return m.setCLIState(*state)
}

func (m *CatalogStateStore) PrepareMigrationsStateStore(_ string) error {
	return nil
}

func (m *CatalogStateStore) GetVersions(database string) (map[uint64]bool, error) {
	state, err := m.getCLIState()
	if err != nil {
		return nil, err
	}
	var versions = map[uint64]bool{}
	for version, dirty := range state.GetMigrationsByDatabase(database) {
		parsedVersion, err := strconv.ParseUint(version, 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parsing migration version")
		}
		versions[parsedVersion] = dirty
	}
	return versions, nil
}
