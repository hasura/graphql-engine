package querycollections

import (
	"io/ioutil"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"gopkg.in/yaml.v3"
)

func TestQueryCollectionConfig_Build(t *testing.T) {
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
		assertErr  require.ErrorAssertionFunc
	}{
		{
			"t1",
			"can build metadata json",
			fields{
				MetadataDir: "testdata/build_test/t1/metadata",
				logger:      logrus.New(),
			},
			"testdata/build_test/t1/want.golden.json",
			false,
			require.NoError,
		},
		{
			"t2",
			"can build metadata json from empty file",
			fields{
				MetadataDir: "testdata/build_test/t2/metadata",
				logger:      logrus.New(),
			},
			"testdata/build_test/t2/want.golden.json",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			q := &QueryCollectionConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := q.Build()
			tt.assertErr(t, err)
			if !tt.wantErr {
				assert.NoError(t, err)
				gotbs, err := yaml.Marshal(got)
				assert.NoError(t, err)
				jsonbs, err := goyaml.YAMLToJSON(gotbs)
				assert.NoError(t, err)

				// uncomment following lines to update golden file
				//assert.NoError(t, ioutil.WriteFile(tt.wantGolden, jsonbs, os.ModePerm))

				wantbs, err := ioutil.ReadFile(tt.wantGolden)
				assert.NoError(t, err)
				assert.Equal(t, string(wantbs), string(jsonbs))
			}
		})
	}
}

func TestQueryCollectionConfig_Export(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata map[string]yaml.Node
	}
	tests := []struct {
		id        string
		name      string
		fields    fields
		args      args
		want      map[string][]byte
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"t1",
			"can export metadata",
			fields{
				MetadataDir: "metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/query_collections.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want.query_collections.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
			require.NoError,
		},
		{
			"t2",
			"can export metadata when query collections is not present in metadata",
			fields{
				MetadataDir: "metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export_test/t2/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/query_collections.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t2/want.query_collections.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
			require.NoError,
		},
		{
			"t3",
			"can export metadata correctly with multiline strings",
			fields{
				MetadataDir: "metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export_test/t3/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/query_collections.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t3/want.query_collections.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
			require.NoError,
		},
		{
			"t4",
			"can export and format metadata correctly on multiline queries",
			fields{
				MetadataDir: "metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export_test/t4/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/query_collections.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t4/want.query_collections.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
			require.NoError,
		},
		{
			"t5",
			"can export yaml correctly when definition.queries is an empty array in json", // see https://github.com/hasura/graphql-engine/issues/8787
			fields{
				MetadataDir: "metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: readJsonFileAndEmitYamlNode(t, "testdata/export_test/t5/metadata.json"),
			},
			map[string][]byte{
				"metadata/query_collections.yaml": readYamlFileAndEmitBytes(t, "testdata/export_test/t5/want.query_collections.yaml"),
			},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			q := &QueryCollectionConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := q.Export(tt.args.metadata)
			tt.assertErr(t, err)
			if !tt.wantErr {
				assert.NoError(t, err)
				for k, v := range got {
					assert.Contains(t, tt.want, k)
					// uncomment to update golden files
					// assert.NoError(t, ioutil.WriteFile(fmt.Sprintf("testdata/export_test/%v/want.%v", tt.id, filepath.Base(k)), v, os.ModePerm))

					assert.Equalf(t, string(tt.want[k]), string(v), "%v", k)
				}
			}
		})
	}
}

func readJsonFileAndEmitYamlNode(t *testing.T, jsonFilePath string) map[string]yaml.Node {
	t.Helper()
	bs, err := ioutil.ReadFile(jsonFilePath)
	assert.NoError(t, err)
	yamlbs, err := metadatautil.JSONToYAML(bs)
	assert.NoError(t, err)
	var v map[string]yaml.Node
	assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
	return v
}

func readYamlFileAndEmitBytes(t *testing.T, yamlFilePath string) []byte {
	t.Helper()
	bs, err := ioutil.ReadFile(yamlFilePath)
	assert.NoError(t, err)
	return bs
}
