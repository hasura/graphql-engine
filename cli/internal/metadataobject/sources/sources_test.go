package sources

import (
	"io/ioutil"
	"testing"

	"github.com/google/go-cmp/cmp"

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
  kind: postgres
  configuration:
    connection_info:
      database_url:
        from_env: HASURA_GRAPHQL_DATABASE_URL
      pool_settings:
        idle_timeout: 180
        max_connections: 50
        retries: 1
      use_prepared_statements: true
  tables: "!include default/tables/tables.yaml"
  functions: "!include default/functions/functions.yaml"
- name: bg
  kind: bigquery
  configuration:
    datasets:
    - t1
    project_id: test_id
    service_account:
      client_email: some_email
      private_key: the private key
      project_id: some_test
  tables: "!include bg/tables/tables.yaml"
`),
				"metadata/databases/default/tables/public_t1.yaml": []byte(`table:
  name: t1
  schema: public
insert_permissions:
- permission:
    backend_only: false
    check:
      id:
        _eq: X-Hasura-User-Id
    columns: []
  role: user
event_triggers:
- definition:
    enable_manual: false
    insert:
      columns: "*"
  name: t1
  retry_conf:
    interval_sec: 10
    num_retries: 0
    timeout_sec: 60
  webhook: https://httpbin.org/post
`),
				"metadata/databases/default/tables/public_t2.yaml": []byte(`table:
  name: t2
  schema: public
`),
				"metadata/databases/default/tables/tables.yaml": []byte(`- "!include public_t1.yaml"
- "!include public_t2.yaml"
`),
				"metadata/databases/default/functions/functions.yaml": []byte(`- "!include public_get_t1.yaml"
- "!include public_get_t2.yaml"
`),
				"metadata/databases/default/functions/public_get_t1.yaml": []byte(`function:
  name: get_t1
  schema: public
some_amazing_stuff:
  test1: test
  test2: test
xyz_test:
  test1: test
  test2: test
`),
				"metadata/databases/default/functions/public_get_t2.yaml": []byte(`function:
  name: get_t2
  schema: public
`),
				"metadata/databases/bg/tables/tables.yaml": []byte(`- "!include london_cycles_cycle_hire.yaml"
- "!include london_cycles_cycle_stations.yaml"
`),
				"metadata/databases/bg/tables/london_cycles_cycle_hire.yaml": []byte(`table:
  dataset: london_cycles
  name: cycle_hire
`),
				"metadata/databases/bg/tables/london_cycles_cycle_stations.yaml": []byte(`table:
  dataset: london_cycles
  name: cycle_stations
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
			assert.NoError(t, err)
			assert.NoError(t, err)
			if diff := cmp.Diff(wantContent, gotContent); diff != "" {
				t.Errorf("Export() mismatch (-want +got):\n%s", diff)
			}
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
      use_prepared_statements: true
  functions:
  - function:
      name: get_t1
      schema: public
  - function:
      name: get_t2
      schema: public
  kind: postgres
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
      use_prepared_statements: true
  functions:
  - function:
      name: get_t1
      schema: public
  - function:
      name: get_t2
      schema: public
  kind: postgres
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
