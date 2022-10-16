package catalogstate

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	errorstestutil "github.com/hasura/graphql-engine/cli/v2/internal/errors/testutil"
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
		name    string
		fields  fields
		args    args
		want    io.Reader
		wantErr *errors.Error
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
			nil,
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
			&errors.Error{
				Op:   "catalogstate.ClientCatalogState.Set",
				Kind: errors.KindHasuraAPI,
				Err: fmt.Errorf(`{
  "code": "parse-failed",
  "error": "When parsing Hasura.RQL.Types.Metadata.Common.CatalogStateType expected a String with the tag of a constructor but got some_state.",
  "path": "$.args.type"
}`),
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := ClientCatalogState{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.Set(tt.args.key, tt.args.state)
			if tt.wantErr == nil {
				assert.NoError(t, err)
				gotb, err := ioutil.ReadAll(got)
				assert.NoError(t, err)
				wantb, err := ioutil.ReadAll(tt.want)
				assert.NoError(t, err)
				assert.JSONEq(t, string(wantb), string(gotb))
			} else {
				errorstestutil.Match(t, os.Stdout, tt.wantErr, err)
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
		name    string
		fields  fields
		want    state
		wantErr *errors.Error
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
			nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := ClientCatalogState{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.Get()
			if tt.wantErr != nil {
				errorstestutil.Match(t, os.Stdout, tt.wantErr, err)
			} else {
				assert.NoError(t, err)
				var gotState state
				assert.NoError(t, json.NewDecoder(got).Decode(&gotState))
				assert.Equal(t, tt.want, gotState)
			}
		})
	}
}
