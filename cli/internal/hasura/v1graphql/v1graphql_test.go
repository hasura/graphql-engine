package v1graphql

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/testutil"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

func TestClient_GetIntrospectionSchema(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}

	tests := []struct {
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
	}{
		{
			"get Introspection Schema from v1.3.3",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "/v1/graphql",
			},
			"v1.3",
			false,
		},
		{
			"get Introspection Schema from latest",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "/v1/graphql",
			},
			"latest",
			false,
		},
		{
			"handles errors gracefully",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "/v1/graphqlsadsa",
			},
			"latest",
			true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.GetIntrospectionSchema()
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				wantb, err := ioutil.ReadFile(filepath.Join("testdata", fmt.Sprintf("%s.golden", tt.wantGolden)))
				require.NoError(t, err)
				gotb, err := json.MarshalIndent(got, "", "  ")
				require.NoError(t, err)
				require.Equal(t, string(wantb), string(gotb))
			}
		})
	}
}
