package migrations

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
)

func TestMigrationStateStoreHdbTable_SetVersions(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	createSchemaMigrations := bytes.NewReader([]byte(`
{
    "type": "run_sql",
    "args": {
        "sql": "CREATE TABLE hdb_catalog.schema_migrations (version bigint not null primary key, dirty boolean not null)"
    }
}
`))
	var body interface{}
	require.NoError(t, json.NewDecoder(createSchemaMigrations).Decode(&body))
	req := testutil.NewRequest(t, http.MethodPost, fmt.Sprintf("%s:%s/%s", testutil.BaseURL, port, "v2/query"), body)
	r, err := http.DefaultClient.Do(req)
	require.NoError(t, err)
	require.Equal(t, r.StatusCode, http.StatusOK)

	type fields struct {
		client hasura.PGSourceOps
		schema string
		table  string
	}
	type args struct {
		sourceName string
		versions   []statestore.Version
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		{
			"can set versions",
			fields{
				postgres.New(testutil.NewHttpcClient(t, port, nil), "v2/query"),
				"hdb_catalog",
				"schema_migrations",
			},
			args{
				"default",
				[]statestore.Version{{Version: 1, Dirty: false}, {Version: 2, Dirty: false}, {Version: 3, Dirty: false}},
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			m := &MigrationStateStoreHdbTable{
				client: tt.fields.client,
				schema: tt.fields.schema,
				table:  tt.fields.table,
			}
			if err := m.SetVersions(tt.args.sourceName, tt.args.versions); (err != nil) != tt.wantErr {
				t.Errorf("SetVersions() error = %v, wantErr %v", err, tt.wantErr)
			}

			versions, err := m.GetVersions(tt.args.sourceName)
			assert.NoError(t, err)
			var got []statestore.Version
			for v, d := range versions {
				got = append(got, statestore.Version{Version: int64(v), Dirty: d})
			}
			assert.ElementsMatch(t, tt.args.versions, got)
		})
	}
}
