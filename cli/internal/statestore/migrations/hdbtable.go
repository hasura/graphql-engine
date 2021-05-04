package migrations

import (
	"fmt"
	"strconv"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/hasura/graphql-engine/cli/migrate/database"
)

// until version 1.4 migration state was stored a special table
// this struct will implement the methods required
type MigrationStateStoreHdbTable struct {
	client        hasura.PGSourceOps
	schema, table string
}

func NewMigrationStateStoreHdbTable(client hasura.PGSourceOps, schema, table string) *MigrationStateStoreHdbTable {
	return &MigrationStateStoreHdbTable{client, schema, table}
}

func (m *MigrationStateStoreHdbTable) InsertVersion(_ string, version int64) error {
	query := hasura.PGRunSQLInput{
		SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", m.schema, m.table) + ` (version, dirty) VALUES (` + strconv.FormatInt(version, 10) + `, ` + fmt.Sprintf("%t", false) + `)`,
	}
	_, err := m.client.PGRunSQL(query)
	if err != nil {
		return err
	}
	return nil
}

func (m *MigrationStateStoreHdbTable) SetVersion(_ string, version int64, dirty bool) error {
	if version >= 0 || (version == database.NilVersion && dirty) {
		query := hasura.PGRunSQLInput{
			SQL: `INSERT INTO ` + fmt.Sprintf("%s.%s", m.schema, m.table) + ` (version, dirty) VALUES (` + strconv.FormatInt(version, 10) + `, ` + fmt.Sprintf("'%t'", dirty) + `)` + fmt.Sprintf(` ON CONFLICT(version) DO UPDATE SET dirty='%t'`, dirty),
		}
		_, err := m.client.PGRunSQL(query)
		if err != nil {
			return err
		}
	}
	return nil
}

func (m *MigrationStateStoreHdbTable) RemoveVersion(_ string, version int64) error {
	query := hasura.PGRunSQLInput{
		SQL: `DELETE FROM ` + fmt.Sprintf("%s.%s", m.schema, m.table) + ` WHERE version = ` + strconv.FormatInt(version, 10),
	}
	_, err := m.client.PGRunSQL(query)
	if err != nil {
		return err
	}
	return nil
}

func (m *MigrationStateStoreHdbTable) PrepareMigrationsStateStore() error {
	// check if migration table exists
	query := hasura.PGRunSQLInput{
		SQL: `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + m.table + `' AND table_schema = '` + m.schema + `' LIMIT 1`,
	}

	runsqlResp, err := m.client.PGRunSQL(query)
	if err != nil {
		return err
	}

	if runsqlResp.ResultType != hasura.TuplesOK {
		return fmt.Errorf("invalid result Type %s", runsqlResp.ResultType)
	}
	result := runsqlResp.Result
	if result[1][0] != "0" {
		return nil
	}

	// Now Create the table
	query = hasura.PGRunSQLInput{
		SQL: `CREATE TABLE ` + fmt.Sprintf("%s.%s", m.schema, m.table) + ` (version bigint not null primary key, dirty boolean not null)`,
	}

	runsqlResp, err = m.client.PGRunSQL(query)
	if err != nil {
		return err
	}
	if runsqlResp.ResultType != hasura.CommandOK {
		return fmt.Errorf("creating Version table failed %s", runsqlResp.ResultType)
	}

	return nil
}

func (m *MigrationStateStoreHdbTable) GetVersions(_ string) (map[uint64]bool, error) {
	query := hasura.PGRunSQLInput{
		SQL: `SELECT version, dirty FROM ` + fmt.Sprintf("%s.%s", m.schema, m.table),
	}

	runsqlResp, err := m.client.PGRunSQL(query)
	if err != nil {
		return nil, err
	}
	if len(runsqlResp.Result) == 1 {
		return nil, nil
	}

	var versions = map[uint64]bool{}
	for index, val := range runsqlResp.Result {
		if index == 0 {
			continue
		}

		version, err := strconv.ParseInt(val[0], 10, 64)
		if err != nil {
			return nil, err
		}
		dirty, err := strconv.ParseBool(val[1])
		if err != nil {
			return nil, err
		}
		versions[uint64(version)] = dirty
		// check if we have to mimic this
		// m.hasuraDB.migrations.Append(database.MigrationVersion{Version: uint64(version), Dirty: dirty})
	}
	return versions, nil
}
