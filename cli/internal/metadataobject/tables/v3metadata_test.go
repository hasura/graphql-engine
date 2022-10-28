package tables

import (
	"io/ioutil"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/stretchr/testify/assert"

	"github.com/sirupsen/logrus"

	"gopkg.in/yaml.v3"
)

func TestV3MetadataTableConfig_Export(t *testing.T) {
	type fields struct {
		TableConfig *TableConfig
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
			id:   "t1",
			name: "can build tables from v3 metadata",
			fields: fields{
				TableConfig: &TableConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/v3_export_test/t1/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			want: map[string][]byte{
				"testdata/metadata/tables.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/v3_export_test/t1/want.tables.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
		},
		{
			id:   "t2",
			name: "can build metadata when tables is empty",
			fields: fields{
				TableConfig: &TableConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/v3_export_test/t2/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			want: map[string][]byte{
				"testdata/metadata/tables.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/v3_export_test/t2/want.tables.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
		},
		{
			id:   "t3",
			name: "can build metadata when sources is not present",
			fields: fields{
				TableConfig: &TableConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/v3_export_test/t3/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			want: map[string][]byte{
				"testdata/metadata/tables.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/v3_export_test/t3/want.tables.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t1 *testing.T) {
			tc := &V3MetadataTableConfig{
				TableConfig: tt.fields.TableConfig,
			}
			got, err := tc.Export(tt.args.metadata)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				for k, v := range got {
					assert.Contains(t, tt.want, k)
					// uncomment to update golden files
					//assert.NoError(t, ioutil.WriteFile(fmt.Sprintf("testdata/v3_export_test/%v/want.%v", tt.id, filepath.Base(k)), v, os.ModePerm))

					assert.Equalf(t, string(tt.want[k]), string(v), "%v", k)
				}
			}
		})
	}
}
