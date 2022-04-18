package remoteschemas

import (
	"io/ioutil"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v3"
)

func TestRemoteSchemaConfig_Build(t *testing.T) {
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
			"can build metadata json",
			fields{
				MetadataDir: "testdata/build_test/t1/metadata",
				logger:      logrus.New(),
			},
			"testdata/build_test/t1/want.golden.json",
			false,
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
		},
		{
			"t3",
			"can build metadata json with multiline strings",
			fields{
				MetadataDir: "testdata/build_test/t3/metadata",
				logger:      logrus.New(),
			},
			"testdata/build_test/t3/want.golden.json",
			false,
		},
		{
			"t4",
			"can build metadata with remote relationships",
			fields{
				MetadataDir: "testdata/build_test/t4/metadata",
				logger:      logrus.New(),
			},
			"testdata/build_test/t4/want.golden.json",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := &RemoteSchemaConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := r.Build()
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				gotbs, err := yaml.Marshal(got)
				assert.NoError(t, err)
				jsonbs, err := goyaml.YAMLToJSON(gotbs)
				assert.NoError(t, err)

				// uncomment following lines to update golden file
				// assert.NoError(t, ioutil.WriteFile(tt.wantGolden, jsonbs, os.ModePerm))

				wantbs, err := ioutil.ReadFile(tt.wantGolden)
				assert.NoError(t, err)
				assert.Equal(t, string(wantbs), string(jsonbs))
			}
		})
	}
}

func TestRemoteSchemaConfig_Export(t *testing.T) {
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
				"metadata/remote_schemas.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want.remote_schemas.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
		},

		{
			"t2",
			"can export metadata",
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
				"metadata/remote_schemas.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t2/want.remote_schemas.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
		},

		{
			"t3",
			"can export remote schema with multiline strings",
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
				"metadata/remote_schemas.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t3/want.remote_schemas.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
		},
		{
			"t4",
			"can export remote schema with multiline strings - 2",
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
				"metadata/remote_schemas.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t4/want.remote_schemas.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
		},
		{
			"t5",
			"can export remote schema with remote relationships",
			fields{
				MetadataDir: "metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export_test/t5/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/remote_schemas.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t5/want.remote_schemas.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := &RemoteSchemaConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := r.Export(tt.args.metadata)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
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
