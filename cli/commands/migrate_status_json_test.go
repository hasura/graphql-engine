package commands

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestStatusToJSON(t *testing.T) {
	newStatus := func(migrations ...*migrate.MigrationStatus) *migrate.Status {
		status := migrate.NewStatus()
		for _, m := range migrations {
			status.Append(m)
		}
		return status
	}

	statusOpts := StatusOptions{
		cli.Source{Name: "pg2"}: newStatus(
			&migrate.MigrationStatus{Version: 3, Name: "add_index", IsPresent: true},
		),
		cli.Source{Name: "default"}: newStatus(
			&migrate.MigrationStatus{Version: 1, Name: "create_users", IsPresent: true, IsApplied: true},
			&migrate.MigrationStatus{Version: 2, Name: "drop_column", IsApplied: true},
		),
	}

	out, err := statusToJSON(statusOpts)
	require.NoError(t, err)
	assert.JSONEq(t, `[
		{"database": "default", "migrations": [
			{"version": 1, "name": "create_users", "present_in_source": true, "applied_on_database": true},
			{"version": 2, "name": "drop_column", "present_in_source": false, "applied_on_database": true}
		]},
		{"database": "pg2", "migrations": [
			{"version": 3, "name": "add_index", "present_in_source": true, "applied_on_database": false}
		]}
	]`, string(out))
}

func TestStatusToJSONWithoutMigrations(t *testing.T) {
	out, err := statusToJSON(StatusOptions{cli.Source{Name: "default"}: migrate.NewStatus()})
	require.NoError(t, err)
	assert.JSONEq(t, `[{"database": "default", "migrations": []}]`, string(out))
}
