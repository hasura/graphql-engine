package sources

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v3"

	"github.com/sirupsen/logrus"
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
				// assert.NoError(t, ioutil.WriteFile(tt.wantGolden, pretty_jsonbs.Bytes(), os.ModePerm))

				wantbs, err := ioutil.ReadFile(tt.wantGolden)
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
				"testdata/export_test/t1/want/databases/databases.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/databases.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/tables/public_t1.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/tables/public_t1.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/tables/public_t2.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/tables/public_t2.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/tables/tables.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/tables/tables.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/functions.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/functions/functions.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/public_get_t1.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/functions/public_get_t1.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/public_get_t2.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/functions/public_get_t2.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/default/functions/test1_test2.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/default/functions/test1_test2.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/bg/tables/tables.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/bg/tables/tables.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_hire.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_hire.yaml")
					assert.NoError(t, err)
					return bs
				}(),
				"testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_stations.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want/databases/bg/tables/london_cycles_cycle_stations.yaml")
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
					// assert.NoError(t, ioutil.WriteFile(fmt.Sprintf("%s", k), v, os.ModePerm))

					assert.Equal(t, string(tt.want[k]), string(v), "%s", k)
				}
			}
		})
	}
}
