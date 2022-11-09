package statestore

import (
	"io/ioutil"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/catalogstate"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestClientCatalogState_GetCLIState(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name      string
		fields    fields
		want      CLIState
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can get catalog state",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/metadata",
			},
			CLIState{
				Migrations: MigrationsState{
					"test": {
						"123": true,
					},
				},
			},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := NewCLICatalogState(catalogstate.New(tt.fields.Client, tt.fields.path))
			_, err := c.Set(CLIState{
				Migrations: MigrationsState{
					"test": {
						"123": true,
					},
				},
			})
			assert.NoError(t, err)
			got, err := c.Get()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			assert.Equal(t, tt.want, *got)
		})
	}
}

func TestCLICatalogState_Set(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		client hasura.CatalogStateOperations
	}
	type args struct {
		state CLIState
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can set CLI state",
			fields{
				client: catalogstate.New(testutil.NewHttpcClient(t, port, nil), "v1/metadata"),
			},
			args{
				state: CLIState{
					Migrations: MigrationsState{
						"test": map[string]bool{
							"123": false,
						},
					},
					Settings: nil,
				},
			},
			`{
  "message": "success"
}`,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &CLICatalogState{
				client: tt.fields.client,
			}
			got, err := c.Set(tt.args.state)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			b, err := ioutil.ReadAll(got)
			assert.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
		})
	}
}
