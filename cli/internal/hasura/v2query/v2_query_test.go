package v2query

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/hasura/commonmetadata"
	"github.com/hasura/graphql-engine/cli/internal/hasura/databaseops"
	"github.com/hasura/graphql-engine/cli/internal/httpc"
	"github.com/hasura/graphql-engine/cli/internal/testutil"
	"github.com/stretchr/testify/assert"
)

func TestClient_Send(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardown()
	type fields struct {
		Client                       *httpc.Client
		path                         string
		HasuraDatabaseRequests       *databaseops.ClientDatabaseOps
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
				Client: func() *httpc.Client {
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", port), nil)
					if err != nil {
						t.Fatal(err)
					}
					return c
				}(),
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
			`{"result_type":"TuplesOk","result":[["?column?"],["1"]]}`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client:            tt.fields.Client,
				path:              tt.fields.path,
				ClientDatabaseOps: tt.fields.HasuraDatabaseRequests,
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
			assert.Equal(t, tt.wantJSONResponseBody, string(b))
		})
	}
}
