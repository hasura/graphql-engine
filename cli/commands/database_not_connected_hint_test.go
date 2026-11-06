package commands

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDatabaseNotConnectedHint(t *testing.T) {
	hint := databaseNotConnectedHint("default")

	assert.Contains(t, hint, "database 'default' is not connected to Hasura")
	assert.Contains(t, hint, "Data > Connect Database")
	assert.Contains(t, hint, "HASURA_GRAPHQL_DATABASE_URL")
	assert.Contains(t, hint, "hasura metadata export")
}
