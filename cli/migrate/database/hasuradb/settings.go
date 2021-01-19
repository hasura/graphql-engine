package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/client"
)

const (
	DefaultSettingsTable = "migration_settings"
)

type SettingsStateStore interface {
	// Get Current setting from database
	GetSetting(name string) (value string, err error)

	// UpdateSetting updates a setting in database.
	UpdateSetting(name string, value string) error

	PrepareSettingsDriver() error
}

func (h *HasuraDB) GetSetting(name string) (value string, err error) {
	return h.settingsStateStore.GetSetting(name)
}

func (h *HasuraDB) UpdateSetting(name string, value string) error {
	return h.settingsStateStore.UpdateSetting(name, value)
}

type SettingsStateStoreWithSQL struct {
	hasuradb *HasuraDB
}

func NewSettingsStateStoreWithSQL(hasuradb *HasuraDB) *SettingsStateStoreWithSQL {
	return &SettingsStateStoreWithSQL{hasuradb}
}

func (s SettingsStateStoreWithSQL) GetSetting(name string) (value string, err error) {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `SELECT value from ` + fmt.Sprintf("%s.%s", DefaultSchema, s.hasuradb.config.SettingsTable) + ` where setting='` + name + `'`,
		},
	}

	// Send Query
	resp, body, err := s.hasuradb.SendMetadataOrQueryRequest(query, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
	if err != nil {
		return value, err
	}
	s.hasuradb.logger.Debug("response: ", string(body))

	// If status != 200 return error
	if resp.StatusCode != http.StatusOK {
		return value, NewHasuraError(body, s.hasuradb.config.isCMD)
	}

	var hres HasuraSQLRes
	err = json.Unmarshal(body, &hres)
	if err != nil {
		return value, err
	}

	if hres.ResultType != TuplesOK {
		return value, fmt.Errorf("Invalid result Type %s", hres.ResultType)
	}

	if len(hres.Result) < 2 {
		for _, setting := range s.hasuradb.settings {
			if setting.GetName() == name {
				return setting.GetDefaultValue(), nil
			}
		}
		return value, fmt.Errorf("Invalid setting name: %s", name)
	}

	return hres.Result[1][0], nil
}

func (s SettingsStateStoreWithSQL) UpdateSetting(name string, value string) error {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", DefaultSchema, s.hasuradb.config.SettingsTable) + ` (setting, value) VALUES ('` + name + `', '` + value + `') ON CONFLICT (setting) DO UPDATE SET value='` + value + `'`,
		},
	}

	// Send Query
	resp, body, err := s.hasuradb.SendMetadataOrQueryRequest(query, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
	if err != nil {
		return err
	}
	s.hasuradb.logger.Debug("response: ", string(body))

	// If status != 200 return error
	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, s.hasuradb.config.isCMD)
	}

	var hres HasuraSQLRes
	err = json.Unmarshal(body, &hres)
	if err != nil {
		return err
	}

	if hres.ResultType != CommandOK {
		return fmt.Errorf("Cannot set setting %s to %s", name, value)
	}
	return nil
}

func (s SettingsStateStoreWithSQL) PrepareSettingsDriver() error {
	h := s.hasuradb
	// check if migration table exists
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + h.config.SettingsTable + `' AND table_schema = '` + DefaultSchema + `' LIMIT 1`,
		},
	}

	resp, body, err := s.hasuradb.SendMetadataOrQueryRequest(query, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
	}

	var hres HasuraSQLRes

	err = json.Unmarshal(body, &hres)
	if err != nil {
		h.logger.Debug(err)
		return err
	}

	if hres.ResultType != TuplesOK {
		return fmt.Errorf("Invalid result Type %s", hres.ResultType)
	}

	if hres.Result[1][0] != "0" {
		return nil
	}

	// Now Create the table
	query = HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `CREATE TABLE ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.SettingsTable) + ` (setting text not null primary key, value text not null)`,
		},
	}

	resp, body, err = s.hasuradb.SendMetadataOrQueryRequest(query, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
	if err != nil {
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
	}

	err = json.Unmarshal(body, &hres)
	if err != nil {
		return err
	}

	if hres.ResultType != CommandOK {
		return fmt.Errorf("Creating Version table failed %s", hres.ResultType)
	}
	return s.setDefaults()
}

func (s SettingsStateStoreWithSQL) setDefaults() error {
	query := HasuraBulk{
		Type: "bulk",
		Args: make([]HasuraQuery, 0),
	}
	for _, setting := range s.hasuradb.settings {
		sql := HasuraQuery{
			Type: "run_sql",
			Args: HasuraArgs{
				SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", DefaultSchema, s.hasuradb.config.SettingsTable) + ` (setting, value) VALUES ('` + fmt.Sprintf("%s", setting.GetName()) + `', '` + fmt.Sprintf("%s", setting.GetDefaultValue()) + `')`,
			},
		}
		query.Args = append(query.Args, sql)
	}

	if len(query.Args) == 0 {
		return nil
	}

	resp, body, err := s.hasuradb.SendMetadataOrQueryRequest(query, &client.MetadataOrQueryClientFuncOpts{MetadataRequestOpts: &client.MetadataRequestOpts{}})
	if err != nil {
		return err
	}

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, s.hasuradb.config.isCMD)
	}

	return nil

}

type SettingsStateStoreWithCatalogStateAPI struct {
	hasuraDB *HasuraDB
}

func NewSettingsStateStoreWithCatalogStateAPI(hasuradb *HasuraDB) *SettingsStateStoreWithCatalogStateAPI {
	return &SettingsStateStoreWithCatalogStateAPI{hasuradb}
}

func (s SettingsStateStoreWithCatalogStateAPI) GetSetting(name string) (value string, err error) {
	catalogStateAPI := client.NewCatalogStateAPI(client.DefaultCLIStateKey)
	catalogState, err := catalogStateAPI.GetCLICatalogState(s.hasuraDB)
	if err != nil {
		return "", err
	}
	v, ok := catalogState.Settings[name]
	if !ok {
		return "", fmt.Errorf("not found")
	}
	return v, nil
}

func (s SettingsStateStoreWithCatalogStateAPI) UpdateSetting(name string, value string) error {
	// get setting
	catalogStateAPI := client.NewCatalogStateAPI(client.DefaultCLIStateKey)
	cliState, err := catalogStateAPI.GetCLICatalogState(s.hasuraDB)
	if err != nil {
		return err
	}
	cliState.Settings[name] = value
	return catalogStateAPI.SetCLICatalogState(s.hasuraDB, *cliState)
}

func (s SettingsStateStoreWithCatalogStateAPI) PrepareSettingsDriver() error {
	return s.setDefaults()
}

func (s SettingsStateStoreWithCatalogStateAPI) setDefaults() error {
	// get setting
	catalogStateAPI := client.NewCatalogStateAPI(client.DefaultCLIStateKey)
	cliState, err := catalogStateAPI.GetCLICatalogState(s.hasuraDB)
	if err != nil {
		return err
	}
	if len(cliState.Settings) == 0 {
		cliState.Settings = make(map[string]string)
	}
	for _, setting := range s.hasuraDB.settings {
		cliState.Settings[setting.GetName()] = setting.GetDefaultValue()
	}
	return catalogStateAPI.SetCLICatalogState(s.hasuraDB, *cliState)
}
