package v1graphql

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

func TestClient_GetIntrospectionSchema(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "hasura/graphql-engine:v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}

	tests := []struct {
		name         string
		fields       fields
		wantGolden   string
		wantErr      bool
		errAssertion require.ErrorAssertionFunc
	}{
		{
			"get Introspection Schema from hasura/graphql-engine:v1.3.3",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "/v1/graphql",
			},
			"v1.3",
			false,
			require.NoError,
		},
		{
			"get Introspection Schema from latest",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "/v1/graphql",
			},
			"latest",
			false,
			require.NoError,
		},
		{
			"handles errors gracefully",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "/v1/graphqlsadsa",
			},
			"latest",
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, e error, i ...interface{}) {
				err, ok := e.(*errors.Error)
				require.True(tt, ok)
				require.Equal(tt, errors.Op("v1graphql.Client.GetIntrospectionSchema"), err.Op)
				require.Equal(tt, errors.KindHasuraAPI.String(), errors.GetKind(err).String())
				require.EqualError(tt, err.Err, ` getIntrospectionSchema : 404 
{
  "code": "not-found",
  "error": "resource does not exist",
  "path": "$"
}`)
			}),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.GetIntrospectionSchema()
			tt.errAssertion(t, err)
			if !tt.wantErr {
				wantb, err := ioutil.ReadFile(filepath.Join("testdata", fmt.Sprintf("%s.golden", tt.wantGolden)))
				require.NoError(t, err)
				gotb, err := json.MarshalIndent(got, "", "  ")
				require.NoError(t, err)
				require.JSONEq(t, string(wantb), string(gotb))
			}
		})
	}
}
