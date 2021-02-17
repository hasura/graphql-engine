package statestore

import (
	"encoding/json"
	"io"

	"github.com/hasura/graphql-engine/cli/internal/hasura"
)

// Abstraction for the storage layer for migration state
type MigrationsStateStore interface {
	InsertVersion(datasource string, version int64) error
	RemoveVersion(datasource string, version int64) error
	SetVersion(datasource string, version int64, dirty bool) error
	GetVersions(datasource string) (map[uint64]bool, error)

	PrepareMigrationsStateStore() error
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
	var state struct {
		CLIState *CLIState `json:"cli_state"`
	}
	b, err := c.client.Get()
	if err != nil {
		return nil, err
	}
	if err := json.NewDecoder(b).Decode(&state); err != nil {
		return nil, err
	}
	return state.CLIState, nil
}

func (c *CLICatalogState) Set(state CLIState) (io.Reader, error) {
	return c.client.Set("cli", state)
}

//
// "default:
//		Version			     Dirty
//		--------------------------
//		"12321312321321321": true
type MigrationsState map[string]map[string]bool

type CLIState struct {
	Migrations MigrationsState   `json:"migrations" mapstructure:"migrations"`
	Settings   map[string]string `json:"settings" mapstructure:"settings"`
}

func (c *CLIState) Init() {
	if c.Migrations == nil {
		c.Migrations = map[string]map[string]bool{}
	}
	if c.Settings == nil {
		c.Settings = map[string]string{}
	}
}
func (c *CLIState) SetMigration(datasource, key string, value bool) {
	if c.Migrations[datasource] == nil {
		c.Migrations[datasource] = map[string]bool{}
	}
	c.Migrations[datasource][key] = value
}

func (c *CLIState) UnsetMigration(datasource, key string) {
	delete(c.Migrations[datasource], key)
}

func (c *CLIState) GetMigrationsByDatasource(datasource string) map[string]bool {
	return c.Migrations[datasource]
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

func CopyMigrationState(src, dest MigrationsStateStore, srcdatasource, destdatasource string) error {
	versions, err := src.GetVersions(srcdatasource)
	if err != nil {
		return err
	}
	for k, v := range versions {
		dest.SetVersion(destdatasource, int64(k), v)
	}
	return nil
}

func CopySettingsState(src, dest SettingsStateStore) error {
	settings, err := src.GetAllSettings()
	if err != nil {
		return err
	}
	for k, v := range settings {
		err := dest.UpdateSetting(k, v)
		if err != nil {
			return err
		}
	}
	return nil
}
