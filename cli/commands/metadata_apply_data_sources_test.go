package commands

import (
	"encoding/json"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/sources"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"gopkg.in/yaml.v3"
)

func TestAddSourceCommandName(t *testing.T) {
	tests := []struct {
		kind string
		want string
	}{
		{"postgres", "pg_add_source"},
		{"mssql", "mssql_add_source"},
		{"citus", "citus_add_source"},
		{"cockroach", "cockroach_add_source"},
		{"bigquery", "bigquery_add_source"},
		{"snowflake", "snowflake_add_source"}, // data connector
	}
	for _, tt := range tests {
		assert.Equal(t, tt.want, addSourceCommandName(tt.kind))
	}
}

func TestBuildAddSourceBulkRequest(t *testing.T) {
	// Mirrors the in-memory shape produced by sources.SourceConfig.Build():
	// a list of sources where tables/functions have already been resolved.
	const databasesYAML = `
- name: default
  kind: postgres
  configuration:
    connection_info:
      database_url:
        from_env: HASURA_GRAPHQL_DATABASE_URL
      use_prepared_statements: true
  customization:
    root_fields:
      namespace: app
  tables:
    - table:
        schema: public
        name: users
    - table:
        schema: public
        name: orders
  functions:
    - function:
        schema: public
        name: search_users
- name: warehouse
  kind: bigquery
  configuration:
    service_account:
      from_env: BIGQUERY_SA
    project_id: my-project
    datasets:
      - analytics
  tables:
    - table:
        dataset: analytics
        name: events
`
	var projectSources []sources.Source
	require.NoError(t, yaml.Unmarshal([]byte(databasesYAML), &projectSources))

	jsonBody, n, err := buildAddSourceBulkRequest(projectSources)
	require.NoError(t, err)
	assert.Equal(t, 2, n)

	// Decode into a generic structure so we can assert on the produced request precisely.
	var bulk struct {
		Type string `json:"type"`
		Args []struct {
			Type string                 `json:"type"`
			Args map[string]interface{} `json:"args"`
		} `json:"args"`
	}
	require.NoError(t, json.Unmarshal(jsonBody, &bulk))

	assert.Equal(t, "bulk", bulk.Type)
	require.Len(t, bulk.Args, 2)

	// Source 1: postgres -> pg_add_source, config + customization preserved, tables/functions excluded.
	pg := bulk.Args[0]
	assert.Equal(t, "pg_add_source", pg.Type)
	assert.Equal(t, "default", pg.Args["name"])
	assert.Equal(t, true, pg.Args["replace_configuration"])
	assert.Contains(t, pg.Args, "configuration")
	assert.Contains(t, pg.Args, "customization")
	assert.NotContains(t, pg.Args, "tables", "tables must never be sent by apply-data-sources")
	assert.NotContains(t, pg.Args, "functions", "functions must never be sent by apply-data-sources")
	// connection configuration (including from_env) is carried through faithfully
	cfg, ok := pg.Args["configuration"].(map[string]interface{})
	require.True(t, ok)
	connInfo, ok := cfg["connection_info"].(map[string]interface{})
	require.True(t, ok)
	dbURL, ok := connInfo["database_url"].(map[string]interface{})
	require.True(t, ok)
	assert.Equal(t, "HASURA_GRAPHQL_DATABASE_URL", dbURL["from_env"])

	// Source 2: bigquery -> bigquery_add_source, no customization key (was absent), tables excluded.
	bq := bulk.Args[1]
	assert.Equal(t, "bigquery_add_source", bq.Type)
	assert.Equal(t, "warehouse", bq.Args["name"])
	assert.Equal(t, true, bq.Args["replace_configuration"])
	assert.NotContains(t, bq.Args, "customization")
	assert.NotContains(t, bq.Args, "tables")
}

func TestBuildAddSourceBulkRequest_MissingKind(t *testing.T) {
	projectSources := []sources.Source{
		{SourceWithNormalFields: sources.SourceWithNormalFields{Name: "default"}},
	}
	_, _, err := buildAddSourceBulkRequest(projectSources)
	assert.Error(t, err)
}
