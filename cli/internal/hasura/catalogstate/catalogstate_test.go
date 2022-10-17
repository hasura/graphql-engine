package catalogstate

import (
	"encoding/json"
	"io"
	"io/ioutil"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

func TestClientCatalogState_Set(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()

	type fields struct {
		Client *httpc.Client
		path   string
	}
	type args struct {
		key   string
		state interface{}
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		want      io.Reader
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can set catalog state",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/metadata",
			},
			args{
				key: "cli",
				state: map[string]string{
					"test": "test",
				},
			},
			strings.NewReader(`{
  "message": "success"
}`),
			false,
			require.NoError,
		},
		{
			"throws an eror on an invalid state type",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/metadata",
			},
			args{
				key: "some_state",
				state: map[string]string{
					"test": "test",
				},
			},
			nil,
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				require.Equal(t, errors.Op("catalogstate.ClientCatalogState.Set"), err.(*errors.Error).Op)
				require.Equal(t, errors.KindHasuraAPI.String(), errors.GetKind(err).String())
				require.Equal(t, err.(*errors.Error).Err.Error(), `{
  "code": "parse-failed",
  "error": "When parsing Hasura.RQL.Types.Metadata.Common.CatalogStateType expected a String with the tag of a constructor but got some_state.",
  "path": "$.args.type"
}`)
			}),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := ClientCatalogState{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.Set(tt.args.key, tt.args.state)
			tt.assertErr(t, err)
			if !tt.wantErr {
				assert.NoError(t, err)
				gotb, err := ioutil.ReadAll(got)
				assert.NoError(t, err)
				wantb, err := ioutil.ReadAll(tt.want)
				assert.NoError(t, err)
				assert.JSONEq(t, string(wantb), string(gotb))
			}
		})
	}
}

func TestClientCatalogState_Get(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()

	type fields struct {
		Client *httpc.Client
		path   string
	}
	type state struct {
		CLIState     map[string]string `json:"cli_state"`
		ConsoleState map[string]string `json:"console_state"`
	}
	tests := []struct {
		name      string
		fields    fields
		want      state
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can get catalog state",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/metadata",
			},
			func() state {
				s := state{
					CLIState:     map[string]string{},
					ConsoleState: map[string]string{},
				}
				return s
			}(),
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := ClientCatalogState{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.Get()
			tt.assertErr(t, err)
			if !tt.wantErr {
				assert.NoError(t, err)
				var gotState state
				assert.NoError(t, json.NewDecoder(got).Decode(&gotState))
				assert.Equal(t, tt.want, gotState)
			}
		})
	}
}
