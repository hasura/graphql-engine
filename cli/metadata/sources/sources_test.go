package sources

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v2"

	"github.com/sirupsen/logrus"
)

func TestSourceConfig_Export(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata yaml.MapSlice
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    map[string][]byte
		wantErr bool
	}{
		{
			"can create sources metadata representation",
			fields{
				MetadataDir: "./metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() yaml.MapSlice {
					var metadata yaml.MapSlice
					jsonb, err := ioutil.ReadFile("testdata/metadata.json")
					assert.NoError(t, err)
					assert.NoError(t, yaml.Unmarshal(jsonb, &metadata))
					return metadata
				}(),
			},
			map[string][]byte{
				"metadata/databases/databases.yaml": []byte(`- name: default
  configuration:
    connection_info:
      database_url:
        from_env: HASURA_GRAPHQL_DATABASE_URL
      pool_settings:
        idle_timeout: 180
        max_connections: 50
        retries: 1
  tables:
  - "!include public_t1.yaml"
  - "!include public_t2.yaml"
  functions:
  - "!include public_get_t1.yaml"
  - "!include public_get_t2.yaml"
`),
				"metadata/databases/default/tables/public_t1.yaml": []byte(`table:
  name: t1
  schema: public
`),
				"metadata/databases/default/tables/public_t2.yaml": []byte(`table:
  name: t2
  schema: public
`),
				"metadata/databases/default/functions/public_get_t1.yaml": []byte(`function:
  name: get_t1
  schema: public
`),
				"metadata/databases/default/functions/public_get_t2.yaml": []byte(`function:
  name: get_t2
  schema: public
`),
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &SourceConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := tc.Export(tt.args.metadata)
			if (err != nil) != tt.wantErr {
				t.Fatalf("Export() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			var wantContent = map[string]string{}
			var gotContent = map[string]string{}
			for k, v := range got {
				gotContent[k] = string(v)
			}
			for k, v := range tt.want {
				wantContent[k] = string(v)
			}
			assert.Equal(t, wantContent, gotContent)
		})
	}
}

func TestSourceConfig_Build(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata *yaml.MapSlice
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    string
		wantErr bool
	}{
		{
			"can build metadata from file",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: new(yaml.MapSlice),
			},
			`sources:
- configuration:
    connection_info:
      database_url:
        from_env: HASURA_GRAPHQL_DATABASE_URL
      pool_settings:
        idle_timeout: 180
        max_connections: 50
        retries: 1
  functions:
  - function:
      name: get_t1
      schema: public
  - function:
      name: get_t2
      schema: public
  name: s1
  tables:
  - table:
      name: t1
      schema: public
  - table:
      name: t2
      schema: public
- configuration:
    connection_info:
      database_url:
        from_env: HASURA_GRAPHQL_DATABASE_URL
      pool_settings:
        idle_timeout: 180
        max_connections: 50
        retries: 1
  functions:
  - function:
      name: get_t1
      schema: public
  - function:
      name: get_t2
      schema: public
  name: s2
  tables:
  - table:
      name: t1
      schema: public
  - table:
      name: t2
      schema: public
`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &SourceConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			if err := tc.Build(tt.args.metadata); (err != nil) != tt.wantErr {
				t.Fatalf("Build() error = %v, wantErr %v", err, tt.wantErr)
			}
			b, err := yaml.Marshal(tt.args.metadata)
			assert.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
		})
	}
}
