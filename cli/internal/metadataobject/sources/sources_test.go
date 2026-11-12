package sources

import (
	"bytes"
	"encoding/json"
	"os"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/sirupsen/logrus"
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.yaml.in/yaml/v3"
)

func TestSourceConfig_Build(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	tests := []struct {
		id         string
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
	}{
		{
			"t1",
			"can build metadata from file",
			fields{
				MetadataDir: "testdata/build_test/t1/metadata",
				logger:      logrus.New(),
			},
			"testdata/build_test/t1/want.golden.json",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &SourceConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := tc.Build()
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				gotbs, err := yaml.Marshal(got)
				assert.NoError(t, err)
				jsonbs, err := goyaml.YAMLToJSON(gotbs)
				assert.NoError(t, err)
				var pretty_jsonbs bytes.Buffer
				err = json.Indent(&pretty_jsonbs, jsonbs, "", " ")
				assert.NoError(t, err)
				// uncomment following lines to update golden file
				// assert.NoError(t, os.WriteFile(tt.wantGolden, pretty_jsonbs.Bytes(), os.ModePerm))

				wantbs, err := os.ReadFile(tt.wantGolden)
				assert.NoError(t, err)
				assert.Equal(t, string(wantbs), pretty_jsonbs.String())
			}
		})
	}
}

func TestSourceConfig_Export(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata map[string]yaml.Node
	}
	tests := []struct {
		id      string
		name    string
		fields  fields
		args    args
		want    map[string][]byte
		wantErr bool
	}{
		{
			"t1",
			"can create sources metadata representation",
			fields{
				MetadataDir: "testdata/export_test/t1/want",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := os.ReadFile("testdata/export_test/t1/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"testdata/export_test/t1/want/databases/databases.yaml": func() []byte {
					bs, err := os.ReadFile("testdata/export_test/t1/want/databases/databases.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/tables/public_t1.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/tables/public_t1.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/tables/public_t2.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/tables/public_t2.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/tables/tables.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/tables/tables.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/functions.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/functions/functions.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/public_get_t1.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/functions/public_get_t1.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/public_get_t2.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/functions/public_get_t2.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/test1_test2.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/default/functions/test1_test2.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/bg/tables/tables.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/bg/tables/tables.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_hire.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_hire.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_stations.yaml": func() []byte {
					bs, err := os.ReadFile(
						"testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_stations.yaml",
					)
					assert.NoError(t, err)
					return bs
				}(),
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
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				for k, v := range got {
					assert.Contains(t, tt.want, k)
					// uncomment to update golden files
					// assert.NoError(t, os.WriteFile(fmt.Sprintf("%s", k), v, os.ModePerm))

					assert.Equal(t, string(tt.want[k]), string(v), "%s", k)
				}
			}
		})
	}
}

func TestPlainTextCredentials(t *testing.T) {
	tests := []struct {
		name   string
		source string
		want   []string
	}{
		{
			"postgres url in plain text",
			`configuration: {connection_info: {database_url: "postgres://user:secret@db:5432/app"}}`,
			[]string{"connection_info.database_url"},
		},
		{
			"postgres url from env",
			`configuration: {connection_info: {database_url: {from_env: PG_DATABASE_URL}}}`,
			nil,
		},
		{
			"postgres url with env templates",
			`configuration: {connection_info: {database_url: "postgres://{{PG_USER}}:{{PG_PASSWORD}}@db:5432/app"}}`,
			nil,
		},
		{
			"postgres url from file",
			`configuration: {connection_info: {database_url: {dynamic_from_file: /secrets/pg_url}}}`,
			nil,
		},
		{
			"postgres connection parameters with password",
			`configuration: {connection_info: {database_url: {connection_parameters: {host: db, password: secret}}}}`,
			[]string{"connection_info.database_url.connection_parameters.password"},
		},
		{
			"postgres connection parameters without password",
			`configuration: {connection_info: {database_url: {connection_parameters: {host: db}}}}`,
			nil,
		},
		{
			"postgres null url",
			`configuration: {connection_info: {database_url: null}}`,
			nil,
		},
		{
			"postgres read replicas and connection set",
			`configuration:
  connection_info: {database_url: {from_env: PG_DATABASE_URL}}
  read_replicas:
    - {database_url: {from_env: PG_REPLICA_URL}}
    - {database_url: "postgres://user:secret@replica:5432/app"}
  connection_set:
    - {name: tenant, connection_info: {database_url: "postgres://user:secret@tenant:5432/app"}}`,
			[]string{
				"read_replicas[1].database_url",
				"connection_set[0].connection_info.database_url",
			},
		},
		{
			"mssql connection string in plain text",
			`configuration: {connection_info: {connection_string: "Server=db;Password=secret"}}`,
			[]string{"connection_info.connection_string"},
		},
		{
			"mssql connection string from env",
			`configuration: {connection_info: {connection_string: {from_env: MSSQL_URL}}}`,
			nil,
		},
		{
			"mssql read replica in plain text",
			`configuration:
  connection_info: {connection_string: {from_env: MSSQL_URL}}
  read_replicas: [{connection_string: "Server=replica;Password=secret"}]`,
			[]string{"read_replicas[0].connection_string"},
		},
		{
			"bigquery service account in plain text",
			`configuration: {service_account: {client_email: a@b.c, private_key: secret, project_id: p}}`,
			[]string{"service_account"},
		},
		{
			"bigquery service account from env",
			`configuration: {service_account: {from_env: BIGQUERY_SERVICE_ACCOUNT}}`,
			nil,
		},
		{
			"no connection info",
			`configuration: {template: null}`,
			nil,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var source struct {
				Configuration yaml.Node `yaml:"configuration"`
			}
			require.NoError(t, yaml.Unmarshal([]byte(tc.source), &source))
			assert.Equal(t, tc.want, plainTextCredentials(source.Configuration))
		})
	}
}

func TestSourceConfig_WarnPlainTextCredentials(t *testing.T) {
	logger, hook := test.NewNullLogger()
	sourceConfig := &SourceConfig{MetadataDir: "metadata", logger: logger}

	metadata := map[string]yaml.Node{}
	require.NoError(t, yaml.Unmarshal([]byte(`
sources:
  - name: default
    kind: postgres
    tables: []
    configuration:
      connection_info:
        database_url: postgres://user:secret@db:5432/app
  - name: other
    kind: postgres
    tables: []
    configuration:
      connection_info:
        database_url:
          from_env: PG_DATABASE_URL
`), &metadata))

	files, err := sourceConfig.Export(metadata)
	require.NoError(t, err)
	assert.Empty(t, hook.AllEntries(), "exporting alone must not warn")

	sourceConfig.WarnPlainTextCredentials(files)

	require.Len(t, hook.AllEntries(), 1)
	assert.Equal(t, logrus.WarnLevel, hook.LastEntry().Level)
	assert.Equal(
		t,
		`database "default": connection_info.database_url is stored in plain text in metadata/databases/databases.yaml; read it from an environment variable (from_env) instead`,
		hook.LastEntry().Message,
	)
}
