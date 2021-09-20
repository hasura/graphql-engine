package v2query

import (
	"io/ioutil"
	"net/http"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/stretchr/testify/require"

	pg "github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/commonmetadata"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/stretchr/testify/assert"
)

func TestClient_Send(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		Client                       *httpc.Client
		path                         string
		HasuraDatabaseRequests       *pg.SourceOps
		HasuraCommonMetadataRequests *commonmetadata.ClientCommonMetadataOps
	}
	type args struct {
		body interface{}
	}
	tests := []struct {
		name                 string
		fields               fields
		args                 args
		wantJSONResponseBody string
		wantErr              bool
	}{
		{
			"can send a request",
			fields{
				Client:                       testutil.NewHttpcClient(t, port, nil),
				path:                         "v2/query",
				HasuraDatabaseRequests:       nil,
				HasuraCommonMetadataRequests: nil,
			},
			args{
				body: map[string]interface{}{
					"type": "run_sql",
					"args": map[string]interface{}{
						"sql": "select 1;",
					},
				},
			},
			`{
  "result_type": "TuplesOk",
  "result": [
    [
      "?column?"
    ],
    [
      "1"
    ]
  ]
}`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			resp, gotResponseBody, err := c.Send(tt.args.body)
			if (err != nil) != tt.wantErr {
				t.Fatalf("Send() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			assert.Equal(t, http.StatusOK, resp.StatusCode)

			b, err := ioutil.ReadAll(gotResponseBody)
			if err != nil {
				t.Fatal(err)
			}
			assert.JSONEq(t, tt.wantJSONResponseBody, string(b))
		})
	}
}

func TestClient_Bulk(t *testing.T) {
	port, mssqlSourceName, teardown := testutil.StartHasuraWithMSSQLSource(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	type args struct {
		args []hasura.RequestBody
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    string
		wantErr bool
	}{
		{
			"can send a bulk request",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v2/query",
			},
			args{
				args: []hasura.RequestBody{
					{
						Type: "mssql_run_sql",
						Args: hasura.PGRunSQLInput{
							SQL:    "select 1",
							Source: mssqlSourceName,
						},
					},
				},
			},
			`[
  {
    "result_type": "TuplesOk",
    "result": [
      [
        ""
      ],
      [
        1
      ]
    ]
  }
]`,
			false,
		},
		{
			"can throw error on a bad request",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/query",
			},
			args{
				args: []hasura.RequestBody{
					{
						Type:    "run_sql",
						Version: 0,
						Args: hasura.PGRunSQLInput{
							SQL: "select something crazy!",
						},
					},
					{
						Type:    "run_sql",
						Version: 0,
						Args: hasura.PGRunSQLInput{
							SQL: "select 1",
						},
					},
				},
			},
			``,
			true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.Bulk(tt.args.args)
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				gotb, err := ioutil.ReadAll(got)
				require.NoError(t, err)
				require.JSONEq(t, tt.want, string(gotb))
			}
		})
	}
}
