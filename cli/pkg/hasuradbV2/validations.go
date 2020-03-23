package hasuradbV2

import (
	"encoding/json"
	"fmt"

	v1 "github.com/hasura/graphql-engine/cli/client/v1"
)

func (db *DB) ensureVersionTable() error {
	// check if migration table exists
	query := v1.SendQueryPayload{
		Type: "run_sql",
		Args: v1.SendQueryPayloadArgs{
			SQL: `SELECT COUNT(1) FROM information_schema.tables WHERE table_name = '` + db.config.MigrationsTable + `' AND table_schema = '` + defaultSchema + `' LIMIT 1`,
		},
	}

	_, body, v1ClientErr := db.v1Client.SendQuery(query)
	// TODO: Verify this will work correctly
	if v1ClientErr != nil {
		return v1ClientErr
	}

	var queryResponse v1.QueryResponse

	var err = json.Unmarshal(body, &queryResponse)
	if err != nil {
		return v1ClientErr
	}

	if queryResponse.ResultType != v1.TuplesOK {
		return fmt.Errorf("Invalid result Type %s", queryResponse.ResultType)
	}
	if queryResponse.Result[1][0] != "0" {
		return nil
	}

	// Now Create the table
	query = v1.SendQueryPayload{
		Type: "run_sql",
		Args: v1.SendQueryPayloadArgs{
			SQL: `CREATE TABLE ` + fmt.Sprintf("%s.%s", defaultSchema, db.config.MigrationsTable) + ` (version bigint not null primary key, dirty boolean not null)`,
		},
	}
	_, body, v1ClientErr = db.v1Client.SendQuery(query)
	if v1ClientErr != nil {
		return v1ClientErr
	}

	err = json.Unmarshal(body, &queryResponse)
	if err != nil {
		return err
	}

	if queryResponse.ResultType != v1.CommandOK {
		return fmt.Errorf("creating Version table failed %s", queryResponse.ResultType)
	}

	return nil
}
