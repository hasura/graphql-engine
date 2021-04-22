package commonmetadata

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/internal/testutil"

	"github.com/hasura/graphql-engine/cli/internal/hasura"
	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

func TestClientCommonMetadataOps_V2ReplaceMetadata(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardown()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	type args struct {
		args hasura.V2ReplaceMetadataArgs
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    hasura.V2ReplaceMetadataResponse
		wantErr bool
	}{
		{
			"can replace with inconsistent metadata",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/metadata",
			},
			args{
				func() hasura.V2ReplaceMetadataArgs {
					metadata := []byte(`{
    "version": 3,
    "sources": [
        {
            "name": "default",
			"kind": "postgres",
           	"tables": [
                        {
                            "table": {
                                "name": "test",
                                "schema": "default"
                            }
                        }
			], 
            "configuration": {
                "connection_info": {
                    "database_url": {
                        "from_env": "HASURA_GRAPHQL_DATABASE_URL"
                    },
                    "pool_settings": {
                        "retries": 1,
                        "idle_timeout": 180,
                        "max_connections": 50
                    },
		    "use_prepared_statements": true
                }
            }
        }
    ]
}`)
					var m interface{}
					assert.NoError(t, json.Unmarshal(metadata, &m))
					var a = hasura.V2ReplaceMetadataArgs{
						AllowInconsistentMetadata: true,
						Metadata:                  m,
					}
					return a
				}(),
			},
			func() hasura.V2ReplaceMetadataResponse {
				wantJson := `{
    "is_consistent": false,
    "inconsistent_objects": [
        {
            "definition": {
                "schema": "default",
                "name": "test"
            },
            "reason": "no such table/view exists in source: \"default.test\"",
            "type": "table"
        }
    ]
}`
				var v2ReplaceMetadataResponse hasura.V2ReplaceMetadataResponse
				assert.NoError(t, json.Unmarshal([]byte(wantJson), &v2ReplaceMetadataResponse))
				return v2ReplaceMetadataResponse
			}(),
			false,
		},

		{
			"can replace with inconsistent metadata",
			fields{
				Client: testutil.NewHttpcClient(t, port, nil),
				path:   "v1/metadata",
			},
			args{
				func() hasura.V2ReplaceMetadataArgs {
					metadata := []byte(`{
    "version": 3,
    "sources": [
        {
            "name": "default",
			"kind": "postgres",
           	"tables": [], 
            "configuration": {
                "connection_info": {
                    "database_url": {
                        "from_env": "HASURA_GRAPHQL_DATABASE_URL"
                    },
                    "pool_settings": {
                        "retries": 1,
                        "idle_timeout": 180,
                        "max_connections": 50
                    },
  		    "use_prepared_statements": true
                }
            }
        }
    ]
}`)
					var m interface{}
					assert.NoError(t, json.Unmarshal(metadata, &m))
					var a = hasura.V2ReplaceMetadataArgs{
						AllowInconsistentMetadata: true,
						Metadata:                  m,
					}
					return a
				}(),
			},
			func() hasura.V2ReplaceMetadataResponse {
				wantJson := `{
    "is_consistent": true,
    "inconsistent_objects": []
}`
				var v2ReplaceMetadataResponse hasura.V2ReplaceMetadataResponse
				assert.NoError(t, json.Unmarshal([]byte(wantJson), &v2ReplaceMetadataResponse))
				return v2ReplaceMetadataResponse
			}(),
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &ClientCommonMetadataOps{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.V2ReplaceMetadata(tt.args.args)
			if !tt.wantErr {
				assert.NoError(t, err)
				assert.Equal(t, tt.want, *got)
			} else {
				assert.Error(t, err)
			}

		})
	}
}
