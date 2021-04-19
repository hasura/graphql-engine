package commonmetadata

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/internal/testutil"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

func TestClient_ExportMetadata(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name          string
		fields        fields
		wantMetadata  string
		wantErr       bool
		hasuraVersion string
	}{
		{
			name: "can export metadata v2",
			wantMetadata: `{
  "version": 2,
  "tables": []
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "v1/query",
			},
			hasuraVersion: "v1.3.3",
			wantErr:       false,
		},
		{
			name: "can export metadata v3",
			wantMetadata: `{
  "version": 3,
  "sources": [
    {
      "name": "default",
      "kind": "postgres",
      "tables": [],
      "configuration": {
        "connection_info": {
          "use_prepared_statements": true,
          "database_url": {
            "from_env": "HASURA_GRAPHQL_DATABASE_URL"
          },
          "pool_settings": {
            "retries": 1,
            "idle_timeout": 180,
            "max_connections": 50
          }
        }
      }
    }
  ]
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "v1/metadata",
			},
			hasuraVersion: testutil.HasuraVersion,
			wantErr:       false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &ClientCommonMetadataOps{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			gotMetadata, err := c.ExportMetadata()
			if (err != nil) != tt.wantErr {
				t.Errorf("ExportMetadata() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			b, err := ioutil.ReadAll(gotMetadata)
			assert.NoError(t, err)
			assert.Equal(t, tt.wantMetadata, string(b))

		})
	}
}

func TestClient_ReloadMetadata(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name          string
		fields        fields
		want          string
		wantErr       bool
		hasuraVersion string
	}{
		{
			name: "can reload metadata v2",
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "v1/query",
			},
			hasuraVersion: "v1.3.3",
			wantErr:       false,
		},
		{
			name: "can reload metadata v3",
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "v1/metadata",
			},
			hasuraVersion: testutil.HasuraVersion,
			wantErr:       false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &ClientCommonMetadataOps{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			gotMetadata, err := c.ReloadMetadata()
			if (err != nil) != tt.wantErr {
				t.Errorf("ReloadMetadata() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			b, err := ioutil.ReadAll(gotMetadata)
			assert.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
		})
	}
}

func TestClient_DropInconsistentMetadata(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name          string
		fields        fields
		want          string
		wantErr       bool
		hasuraVersion string
	}{
		{
			name: "can drop inconsistent metadata v2",
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "v1/query",
			},
			hasuraVersion: "v1.3.3",
			wantErr:       false,
		},
		{
			name: "can drop inconsistent metadata v3",
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "v1/metadata",
			},
			hasuraVersion: testutil.HasuraVersion,
			wantErr:       false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &ClientCommonMetadataOps{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			gotMetadata, err := c.DropInconsistentMetadata()
			if (err != nil) != tt.wantErr {
				t.Errorf("DropInconsistentMetadata() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			b, err := ioutil.ReadAll(gotMetadata)
			assert.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
		})
	}
}

func TestClient_ResetMetadata(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name          string
		fields        fields
		want          string
		wantErr       bool
		hasuraVersion string
	}{
		{
			name: "can reset metadata",
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "v1/query",
			},
			hasuraVersion: "v1.3.3",
			wantErr:       false,
		},
		{
			name: "can reset metadata",
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "v1/metadata",
			},
			hasuraVersion: testutil.HasuraVersion,
			wantErr:       false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Run(tt.name, func(t *testing.T) {
				c := &ClientCommonMetadataOps{
					Client: tt.fields.Client,
					path:   tt.fields.path,
				}
				got, err := c.ClearMetadata()
				if (err != nil) != tt.wantErr {
					t.Errorf("ClearMetadata() error = %v, wantErr %v", err, tt.wantErr)
					return
				}
				b, err := ioutil.ReadAll(got)
				assert.NoError(t, err)
				assert.Equal(t, tt.want, string(b))
			})
		})
	}
}

func TestClient_GetInconsistentMetadata(t *testing.T) {
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	// create a table track it and delete it
	sendReq := func(body io.Reader, url string) {
		resp, err := http.Post(fmt.Sprintf("http://%s:%s/%s", "0.0.0.0", portHasuraLatest, url), "application/json", body)
		assert.NoError(t, err)
		defer resp.Body.Close()
		if err != nil {
			t.Fatal(err)
		}
		if resp.StatusCode != http.StatusOK {
			b, err := ioutil.ReadAll(resp.Body)
			if err != nil {
				t.Fatal(err)
			}
			t.Fatal("request failed", resp.StatusCode, string(b))
		}
	}
	createTable := strings.NewReader(`
{
	"type": "run_sql",
	"args": {
		"sql": "CREATE TABLE test();"
	}
}
`)
	dropTable := strings.NewReader(`
{
	"type": "run_sql",
	"args": {
		"sql": "DROP TABLE test;",
		"check_metadata_consistency": false
	}
}
`)
	trackTable := strings.NewReader(`
{
    "type": "pg_track_table",
    "args": {
        "schema": "public",
        "name": "test"
    }
}

`)
	reloadMetadata := strings.NewReader(`
{
    "type": "reload_metadata",
    "args": {
        "schema": "public",
        "name": "test"
    }
}

`)
	sendReq(createTable, "v2/query")
	sendReq(trackTable, "v1/metadata")
	sendReq(dropTable, "v2/query")
	sendReq(reloadMetadata, "v1/metadata")

	type fields struct {
		Client *httpc.Client
		path   string
	}
	tests := []struct {
		name          string
		fields        fields
		want          io.Reader
		wantErr       bool
		hasuraVersion string
	}{
		{
			name: "can get inconsistent metadata",
			want: bytes.NewReader([]byte(`{"is_consistent":false,"inconsistent_objects":[{"definition":{"schema":"public","name":"test"},"reason":"no such table/view exists in source: \"test\"","type":"table"}]}`)),
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "v1/metadata",
			},
			hasuraVersion: testutil.HasuraVersion,
			wantErr:       false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &ClientCommonMetadataOps{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.GetInconsistentMetadata()
			if (err != nil) != tt.wantErr {
				t.Errorf("ClearMetadata() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			var wantStruct = new(hasura.GetInconsistentMetadataResponse)
			err = json.NewDecoder(tt.want).Decode(wantStruct)
			assert.NoError(t, err)
			assert.NoError(t, err)
			assert.Equal(t, wantStruct, got)
		})
	}
}

func TestClient_ReplaceMetadata(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	type args struct {
		metadata io.Reader
	}
	tests := []struct {
		name          string
		args          args
		fields        fields
		want          string
		wantErr       bool
		hasuraVersion string
	}{
		{
			name: "can replace metadata v2",
			want: `{
  "message": "success"
}`,
			args: args{
				metadata: bytes.NewBuffer([]byte(`{"version":2, "tables":[]}`)),
			},
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "v1/query",
			},
			hasuraVersion: "v1.3.3",
			wantErr:       false,
		},
		{
			name: "can replace metadata V3",
			args: args{
				metadata: bytes.NewBuffer([]byte(`{"version":3, "sources":[]}`)),
			},
			want: `{
  "message": "success"
}`,
			fields: fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "v1/metadata",
			},
			hasuraVersion: testutil.HasuraVersion,
			wantErr:       false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Run(tt.name, func(t *testing.T) {
				c := &ClientCommonMetadataOps{
					Client: tt.fields.Client,
					path:   tt.fields.path,
				}
				got, err := c.ReplaceMetadata(tt.args.metadata)
				if (err != nil) != tt.wantErr {
					t.Errorf("ReplaceMetadata() error = %v, wantErr %v", err, tt.wantErr)
					return
				}
				b, err := ioutil.ReadAll(got)
				assert.NoError(t, err)
				assert.Equal(t, tt.want, string(b))
			})
		})
	}
}
