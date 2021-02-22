package statestore

import (
	"io/ioutil"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/hasura/graphql-engine/cli/internal/hasura/catalogstate"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
	"github.com/hasura/graphql-engine/cli/internal/testutil"
	"github.com/stretchr/testify/assert"
)

func TestClientCatalogState_GetCLIState(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardown()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name    string
		fields  fields
		want    CLIState
		wantErr bool
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
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tt.want, *got)
			}
		})
	}
}

func TestCLICatalogState_Set(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardown()
	type fields struct {
		client hasura.CatalogStateOperations
	}
	type args struct {
		state CLIState
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    string
		wantErr bool
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
			`{"message":"success"}`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &CLICatalogState{
				client: tt.fields.client,
			}
			got, err := c.Set(tt.args.state)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				b, err := ioutil.ReadAll(got)
				assert.NoError(t, err)
				assert.Equal(t, tt.want, string(b))
			}
		})
	}
}
