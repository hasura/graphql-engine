package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"
)

const (
	DefaultSettingsTable = "migration_settings"
)

func (h *HasuraDB) ensureSettingsTable() error {
	// check if migration table exists
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + h.config.SettingsTable + `' AND table_schema = '` + DefaultSchema + `' LIMIT 1`,
		},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return err
		}
		return horror.Error(h.config.isCMD)
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

	resp, body, err = h.sendv1Query(query)
	if err != nil {
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return err
		}

		return horror.Error(h.config.isCMD)
	}

	err = json.Unmarshal(body, &hres)
	if err != nil {
		return err
	}

	if hres.ResultType != CommandOK {
		return fmt.Errorf("Creating Version table failed %s", hres.ResultType)
	}
	return h.setDefaultSettings()
}

func (h *HasuraDB) setDefaultSettings() error {
	query := HasuraBulk{
		Type: "bulk",
		Args: make([]HasuraQuery, 0),
	}
	for _, setting := range h.settings {
		sql := HasuraQuery{
			Type: "run_sql",
			Args: HasuraArgs{
				SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.SettingsTable) + ` (setting, value) VALUES ('` + fmt.Sprintf("%s", setting.GetName()) + `', '` + fmt.Sprintf("%s", setting.GetDefaultValue()) + `')`,
			},
		}
		query.Args = append(query.Args, sql)
	}

	if len(query.Args) == 0 {
		return nil
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		return err
	}

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return err
		}
		return horror.Error(h.config.isCMD)
	}

	return nil
}

func (h *HasuraDB) GetSetting(name string) (value string, err error) {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `SELECT value from ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.SettingsTable) + ` where setting='` + name + `'`,
		},
	}

	// Send Query
	resp, body, err := h.sendv1Query(query)
	if err != nil {
		return value, err
	}
	h.logger.Debug("response: ", string(body))

	var horror HasuraError

	// If status != 200 return error
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return value, err
		}

		return value, horror.Error(h.config.isCMD)
	}

	var hres HasuraSQLRes
	err = json.Unmarshal(body, &hres)
	if err != nil {
		return value, err
	}

	if hres.ResultType != TuplesOK {
		return value, fmt.Errorf("Invalid result Type %s", hres.ResultType)
	}

	if len(hres.Result) == 0 {
		for _, setting := range h.settings {
			if setting.GetName() == name {
				return setting.GetDefaultValue(), nil
			}
		}
		return "", nil
	}

	return hres.Result[1][0], nil
}

func (h *HasuraDB) UpdateSetting(name string, value string) error {
	query := HasuraQuery{
		Type: "run_sql",
		Args: HasuraArgs{
			SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", DefaultSchema, h.config.SettingsTable) + ` (setting, value) VALUES ('` + name + `', '` + value + `') ON CONFLICT (setting) DO UPDATE SET value='` + value + `'`,
		},
	}

	// Send Query
	resp, body, err := h.sendv1Query(query)
	if err != nil {
		return err
	}
	h.logger.Debug("response: ", string(body))

	var horror HasuraError

	// If status != 200 return error
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			return err
		}

		return horror.Error(h.config.isCMD)
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
