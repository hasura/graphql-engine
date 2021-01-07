package hasuradb

import (
	"fmt"
	"net/http"
	"strconv"

	"github.com/hasura/graphql-engine/cli/migrate/database"

	"github.com/hasura/graphql-engine/cli/internal/client"
	"github.com/pkg/errors"
)

// from v1.4 clients are expected to make use of the catalog API
// rather than assuming a SQL backend for metadata storage
type migrationsStateWithCatalogStateAPI struct {
	hasuradb *HasuraDB
}

func NewMigrationsStateWithCatalogStateAPI(hasuraDB *HasuraDB) *migrationsStateWithCatalogStateAPI {
	return &migrationsStateWithCatalogStateAPI{
		hasuradb: hasuraDB,
	}
}

func (m *migrationsStateWithCatalogStateAPI) InsertVersion(version int64) error {
	// get setting
	catalogStateAPI := client.NewCatalogStateAPI(client.DefaultCLIStateKey)
	cliState, err := catalogStateAPI.GetCLICatalogState(m.hasuradb)
	if err != nil {
		return err
	}
	versionString := fmt.Sprintf("%d", version)
	if cliState.Migrations == nil {
		cliState.Migrations = client.MigrationsState{}
	}
	if cliState.Migrations[m.hasuradb.hasuraOpts.Datasource] == nil {
		cliState.Migrations[m.hasuradb.hasuraOpts.Datasource] = map[string]bool{}
	}

	cliState.Migrations[m.hasuradb.hasuraOpts.Datasource][versionString] = false
	q := HasuraInterfaceQuery{
		Type: "set_catalog_state",
		Args: struct {
			Type  string                 `json:"type"`
			State client.CLICatalogState `json:"state"`
		}{
			Type:  "cli",
			State: *cliState,
		},
	}
	return m.makeMetadataAPICall(q)
}
func (m *migrationsStateWithCatalogStateAPI) makeMetadataAPICall(body interface{}) error {
	if resp, body, err := m.hasuradb.SendMetadataOrQueryRequest(body, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}}); err != nil {
		return err
	} else if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("hasura api error: %s", errors.New(string(body)))
	}
	return nil
}
func (m *migrationsStateWithCatalogStateAPI) SetVersion(version int64, dirty bool) error {
	// get setting
	catalogStateAPI := client.NewCatalogStateAPI(client.DefaultCLIStateKey)
	cliState, err := catalogStateAPI.GetCLICatalogState(m.hasuradb)
	if err != nil {
		return err
	}
	versionString := fmt.Sprintf("%d", version)
	if cliState.Migrations == nil {
		cliState.Migrations = client.MigrationsState{}
	}
	if cliState.Migrations[m.hasuradb.hasuraOpts.Datasource] == nil {
		cliState.Migrations[m.hasuradb.hasuraOpts.Datasource] = map[string]bool{}
	}

	cliState.Migrations[m.hasuradb.hasuraOpts.Datasource][versionString] = dirty
	q := HasuraInterfaceQuery{
		Type: "set_catalog_state",
		Args: struct {
			Type  string                 `json:"type"`
			State client.CLICatalogState `json:"state"`
		}{
			Type:  "cli",
			State: *cliState,
		},
	}
	return m.makeMetadataAPICall(q)
}

func (m *migrationsStateWithCatalogStateAPI) RemoveVersion(version int64) error {
	cliState := *m.hasuradb.CLICatalogState
	delete(cliState.Migrations[m.hasuradb.hasuraOpts.Datasource], fmt.Sprintf("%d", version))

	q := HasuraInterfaceQuery{
		Type: "set_catalog_state",
		Args: struct {
			Type  string                 `json:"type"`
			State client.CLICatalogState `json:"state"`
		}{
			Type:  "cli",
			State: cliState,
		},
	}
	return m.makeMetadataAPICall(q)
}

func (m *migrationsStateWithCatalogStateAPI) PrepareMigrationsStateStore() error {
	return nil
}

func (m *migrationsStateWithCatalogStateAPI) GetVersions() error {
	catalogStateAPI := client.NewCatalogStateAPI("cli_state")
	state, err := catalogStateAPI.GetCLICatalogState(m.hasuradb)
	if err != nil {
		return err
	}
	v, ok := state.Migrations[m.hasuradb.hasuraOpts.Datasource]
	if !ok {
		return nil
	}
	for version, dirty := range v {
		parsedVersion, err := strconv.ParseUint(version, 10, 64)
		if err != nil {
			return errors.Wrap(err, "parsing migration version")
		}
		m.hasuradb.migrations.Append(database.MigrationVersion{Version: parsedVersion, Dirty: dirty})
	}
	return nil
}
