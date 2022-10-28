package v1query

import (
	"io/ioutil"
	"net/http"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"
	pg "github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/commonmetadata"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/stretchr/testify/assert"
)

func TestClient_Send(t *testing.T) {
	port, teardown := testutil.StartHasura(t, "hasura/graphql-engine:v1.3.3")
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
		assertResponse       assert.ComparisonAssertionFunc
		wantCode             int
		assertResponseCode   assert.ComparisonAssertionFunc
		wantErr              require.ErrorAssertionFunc
	}{
		{
			"can send a request",
			fields{
				Client:                       testutil.NewHttpcClient(t, port, nil),
				path:                         "v1/query",
				HasuraDatabaseRequests:       nil,
				HasuraCommonMetadataRequests: nil,
			},
			args{
				body: map[string]string{
					"type": "export_metadata",
					"args": "{}",
				},
			},
			`{
  "version": 2,
  "tables": []
}`,
			assert.ComparisonAssertionFunc(func(tt assert.TestingT, i1, i2 interface{}, i3 ...interface{}) bool {
				return assert.JSONEq(t, i1.(string), i2.(string))
			}),
			http.StatusOK,
			assert.Equal,
			require.NoError,
		},
		{
			"can return right error type",
			fields{
				Client:                       testutil.NewHttpcClient(t, port, nil),
				path:                         "v1/query",
				HasuraDatabaseRequests:       nil,
				HasuraCommonMetadataRequests: nil,
			},
			args{
				body: map[string]string{
					"type": "export_metadata2",
					"args": "this is not expected",
				},
			},
			"",
			assert.ComparisonAssertionFunc(func(tt assert.TestingT, _, _ interface{}, _ ...interface{}) bool {
				return true
			}),
			http.StatusBadRequest,
			assert.Equal,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client:                  tt.fields.Client,
				path:                    tt.fields.path,
				SourceOps:               tt.fields.HasuraDatabaseRequests,
				ClientCommonMetadataOps: tt.fields.HasuraCommonMetadataRequests,
			}
			resp, gotResponseBody, err := c.Send(tt.args.body)
			tt.wantErr(t, err)

			tt.assertResponseCode(t, tt.wantCode, resp.StatusCode)

			b, err := ioutil.ReadAll(gotResponseBody)
			assert.NoError(t, err)

			tt.assertResponse(t, tt.wantJSONResponseBody, string(b))
		})
	}
}

func TestClient_Bulk(t *testing.T) {
	port, teardown := testutil.StartHasura(t, "hasura/graphql-engine:v1.3.3")
	defer teardown()
	type fields struct {
		Client                  *httpc.Client
		path                    string
		SourceOps               *postgres.SourceOps
		ClientCommonMetadataOps *commonmetadata.ClientCommonMetadataOps
	}
	type args struct {
		args []hasura.RequestBody
	}
	tests := []struct {
		name           string
		fields         fields
		args           args
		want           string
		assertResponse require.ComparisonAssertionFunc
		assertErr      require.ErrorAssertionFunc
		wantErr        bool
	}{
		{
			"can send a bulk request",
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
							SQL: "select 1",
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
			`[
  {
    "result_type": "TuplesOk",
    "result": [
      [
        "?column?"
      ],
      [
        "1"
      ]
    ]
  },
  {
    "result_type": "TuplesOk",
    "result": [
      [
        "?column?"
      ],
      [
        "1"
      ]
    ]
  }
]`,
			require.ComparisonAssertionFunc(
				func(tt require.TestingT, i1, i2 interface{}, i3 ...interface{}) {
					require.JSONEq(tt, i1.(string), i2.(string))
				}),
			require.NoError,
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
			require.Equal,
			require.ErrorAssertionFunc(
				func(tt require.TestingT, err error, i ...interface{}) {
					require.IsType(t, &errors.Error{}, err)
					require.Equal(tt, errors.KindHasuraAPI.String(), errors.GetKind(err).String())
					require.Equal(tt, errors.Op("v1query.Client.Bulk"), err.(*errors.Error).Op)
					require.Contains(tt, err.(*errors.Error).Err.Error(), "bulk request failed:")
				}),
			true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client:                  tt.fields.Client,
				path:                    tt.fields.path,
				SourceOps:               tt.fields.SourceOps,
				ClientCommonMetadataOps: tt.fields.ClientCommonMetadataOps,
			}
			got, err := c.Bulk(tt.args.args)
			tt.assertErr(t, err)
			if !tt.wantErr {
				gotb, err := ioutil.ReadAll(got)
				require.NoError(t, err)
				require.NotNil(t, got)
				tt.assertResponse(t, tt.want, string(gotb))
			}
		})
	}
}
