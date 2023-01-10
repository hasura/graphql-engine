package settings

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

type StateStoreHdbTable struct {
	client        hasura.PGSourceOps
	sourceName    string
	schema, table string
}

func NewStateStoreHdbTable(client hasura.PGSourceOps, sourceName, schema, table string) *StateStoreHdbTable {
	return &StateStoreHdbTable{client, sourceName, schema, table}
}

func (s *StateStoreHdbTable) GetSetting(name string) (value string, err error) {
	var op errors.Op = "settings.StateStoreHdbTable.GetSetting"
	query := hasura.PGRunSQLInput{
		Source: s.sourceName,
		SQL:    `SELECT value from ` + fmt.Sprintf("%s.%s", s.schema, s.table) + ` where setting='` + name + `'`,
	}

	resp, err := s.client.PGRunSQL(query)
	if err != nil {
		return value, errors.E(op, err)
	}

	if resp.ResultType != hasura.TuplesOK {
		return value, errors.E(op, fmt.Errorf("invalid result Type %s", resp.ResultType))
	}

	if len(resp.Result) < 2 {
		for _, setting := range Settings {
			if setting.GetName() == name {
				return setting.GetDefaultValue(), nil
			}
		}
		return value, errors.E(op, fmt.Errorf("invalid setting name: %s", name))
	}

	return resp.Result[1][0], nil
}

func (s *StateStoreHdbTable) GetAllSettings() (map[string]string, error) {
	var op errors.Op = "settings.StateStoreHdbTable.GetAllSettings"
	query := hasura.PGRunSQLInput{
		Source: s.sourceName,
		SQL:    `SELECT setting, value from ` + fmt.Sprintf("%s.%s", s.schema, s.table) + `;`,
	}

	resp, err := s.client.PGRunSQL(query)
	if err != nil {
		return nil, errors.E(op, err)
	}

	if resp.ResultType != hasura.TuplesOK {
		return nil, errors.E(op, fmt.Errorf("invalid result Type %s", resp.ResultType))
	}

	var settings = map[string]string{}
	for idx, row := range resp.Result {
		if idx == 0 {
			continue
		}
		if len(row) == 2 {
			settings[row[0]] = row[1]
		}
	}
	return settings, nil
}

func (s *StateStoreHdbTable) UpdateSetting(name string, value string) error {
	var op errors.Op = "statestore.StateStoreHdbTable.UpdateSetting"
	query := hasura.PGRunSQLInput{
		Source: s.sourceName,
		SQL:    `INSERT INTO ` + fmt.Sprintf("%s.%s", s.schema, s.table) + ` (setting, value) VALUES ('` + name + `', '` + value + `') ON CONFLICT (setting) DO UPDATE SET value='` + value + `'`,
	}

	resp, err := s.client.PGRunSQL(query)
	if err != nil {
		return errors.E(op, err)
	}
	if resp.ResultType != hasura.CommandOK {
		return errors.E(op, fmt.Errorf("cannot set setting %s to %s", name, value))
	}
	return nil
}

func (s *StateStoreHdbTable) PrepareSettingsDriver() error {
	var op errors.Op = "statestore.StateStoreHdbTable.PrepareSettingsDriver"
	// check if migration table exists
	query := hasura.PGRunSQLInput{
		Source: s.sourceName,
		SQL:    `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + s.table + `' AND table_schema = '` + s.schema + `' LIMIT 1`,
	}

	resp, err := s.client.PGRunSQL(query)
	if err != nil {
		return errors.E(op, err)
	}

	if resp.ResultType != hasura.TuplesOK {
		return errors.E(op, fmt.Errorf("invalid result Type %s", resp.ResultType))
	}

	if resp.Result[1][0] != "0" {
		return nil
	}

	// Now Create the table
	query = hasura.PGRunSQLInput{
		Source: s.sourceName,
		SQL:    `CREATE TABLE ` + fmt.Sprintf("%s.%s", s.schema, s.table) + ` (setting text not null primary key, value text not null)`,
	}

	resp, err = s.client.PGRunSQL(query)
	if err != nil {
		return errors.E(op, err)
	}

	if resp.ResultType != hasura.CommandOK {
		return errors.E(op, fmt.Errorf("creating Version table failed %s", resp.ResultType))
	}
	if err := s.setDefaults(); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (s *StateStoreHdbTable) setDefaults() error {
	var op errors.Op = "statestore.StateStoreHdbTable.setDefaults"
	var sql string
	for _, setting := range Settings {
		sql += `INSERT INTO ` + fmt.Sprintf("%s.%s", s.schema, s.table) + ` (setting, value) VALUES ('` + setting.GetName() + `', '` + setting.GetDefaultValue() + `');`
	}

	query := hasura.PGRunSQLInput{
		Source: s.sourceName,
		SQL:    sql,
	}
	_, err := s.client.PGRunSQL(query)
	if err != nil {
		return errors.E(op, err)
	}

	return nil

}
