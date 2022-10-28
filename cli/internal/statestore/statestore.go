package statestore

import (
	"encoding/json"
	"io"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

type Version struct {
	Version int64
	Dirty   bool
}

// Abstraction for the storage layer for migration state
type MigrationsStateStore interface {
	InsertVersion(database string, version int64) error
	RemoveVersion(database string, version int64) error
	SetVersion(database string, version int64, dirty bool) error
	GetVersions(database string) (map[uint64]bool, error)
	SetVersions(database string, versions []Version) error

	PrepareMigrationsStateStore(database string) error
}

// Abstraction for storage layer of CLI settings
type SettingsStateStore interface {
	GetSetting(name string) (value string, err error)
	UpdateSetting(name string, value string) error
	GetAllSettings() (map[string]string, error)
	PrepareSettingsDriver() error
}

type CLICatalogState struct {
	client hasura.CatalogStateOperations
}

func NewCLICatalogState(client hasura.CatalogStateOperations) *CLICatalogState {
	return &CLICatalogState{client}
}

func (c *CLICatalogState) Get() (*CLIState, error) {
	var op errors.Op = "statestore.CLICatalogState.Get"
	var state struct {
		CLIState *CLIState `json:"cli_state"`
	}
	b, err := c.client.Get()
	if err != nil {
		return nil, errors.E(op, err)
	}
	if err := json.NewDecoder(b).Decode(&state); err != nil {
		return nil, errors.E(op, err)
	}
	return state.CLIState, nil
}

func (c *CLICatalogState) Set(state CLIState) (io.Reader, error) {
	var op errors.Op = "statestore.CLICatalogState.Set"
	r, err := c.client.Set("cli", state)
	if err != nil {
		return r, errors.E(op, err)
	}
	return r, nil
}

//
// "default:
//		Version			     Dirty
//		--------------------------
//		"12321312321321321": true
type MigrationsState map[string]map[string]bool

type CLIState struct {
	Migrations MigrationsState   `json:"migrations,omitempty" mapstructure:"migrations,omitempty"`
	Settings   map[string]string `json:"settings" mapstructure:"settings"`
	// IsStateCopyCompleted is a utility variable
	// pre config v3 state was stored in users database connected to hasura in `hdb_catalog.*` tables
	// this variable is set to true when state copy happens from hdb_catalog.* tables
	// this process is carried out during a scripts update-project-v3 command or an implicit state copy
	// introduced in https://github.com/hasura/graphql-engine-mono/pull/1298
	IsStateCopyCompleted bool `json:"isStateCopyCompleted" mapstructure:"isStateCopyCompleted"`
}

func (c *CLIState) Init() {
	if c.Migrations == nil {
		c.Migrations = map[string]map[string]bool{}
	}
	if c.Settings == nil {
		c.Settings = map[string]string{}
	}
}
func (c *CLIState) SetMigration(database, key string, value bool) {
	if c.Migrations[database] == nil {
		c.Migrations[database] = map[string]bool{}
	}
	c.Migrations[database][key] = value
}

func (c *CLIState) UnsetMigration(database, key string) {
	delete(c.Migrations[database], key)
}

func (c *CLIState) GetMigrationsByDatabase(database string) map[string]bool {
	return c.Migrations[database]
}

func (c *CLIState) GetMigrations() *MigrationsState {
	return &c.Migrations
}

func (c *CLIState) SetSetting(key, value string) {
	if c.Settings == nil {
		c.Settings = map[string]string{}
	}
	c.Settings[key] = value
}

func (c *CLIState) GetSetting(key string) string {
	v, ok := c.Settings[key]
	if !ok {
		return ""
	}
	return v
}

func (c *CLIState) GetSettings() map[string]string {
	return c.Settings
}

func CopyMigrationState(src, dest MigrationsStateStore, srcdatabase, destdatabase string) error {
	var op errors.Op = "statestore.CopyMigrationState"
	versions, err := src.GetVersions(srcdatabase)
	if err != nil {
		return errors.E(op, err)
	}
	var vs []Version
	for v, dirty := range versions {
		vs = append(vs, Version{int64(v), dirty})
	}
	if err := dest.SetVersions(destdatabase, vs); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func CopySettingsState(src, dest SettingsStateStore) error {
	var op errors.Op = "statestore.CopySettingsState"
	settings, err := src.GetAllSettings()
	if err != nil {
		return errors.E(op, err)
	}
	for k, v := range settings {
		err := dest.UpdateSetting(k, v)
		if err != nil {
			return errors.E(op, err)
		}
	}
	return nil
}
