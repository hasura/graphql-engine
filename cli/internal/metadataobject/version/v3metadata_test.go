package version

import (
	"io/ioutil"
	"os"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v3"
)

func TestV3MetadataV2ConfigVersion_Export(t *testing.T) {
	type fields struct {
		VersionConfig *VersionConfig
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
				VersionConfig: &VersionConfig{
					MetadataDir: "metadata",
				},
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/v3_metadata_v2_config_export_test/t1/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/version.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/v3_metadata_v2_config_export_test/t1/want.version.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &V3MetadataV2ConfigVersion{
				VersionConfig: tt.fields.VersionConfig,
			}
			got, err := a.Export(tt.args.metadata)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				for k, v := range got {
					assert.Contains(t, tt.want, k)
					// uncomment to update golden files
					//assert.NoError(t, ioutil.WriteFile(fmt.Sprintf("testdata/v3_metadata_v2_config_export_test/%v/want.%v", tt.id, filepath.Base(k)), v, os.ModePerm))

					assert.Equalf(t, string(tt.want[k]), string(v), "%v", k)
				}
			}
		})
	}
}

func TestV3MetadataV2ConfigVersion_Build(t *testing.T) {
	type fields struct {
		VersionConfig *VersionConfig
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
				VersionConfig: &VersionConfig{
					MetadataDir: "testdata/v3_metadata_v2_config_build_test/t1/metadata",
				},
			},
			"testdata/v3_metadata_v2_config_build_test/t1/want.golden.json",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &V3MetadataV2ConfigVersion{
				VersionConfig: tt.fields.VersionConfig,
			}
			got, err := a.Build()
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				gotbs, err := yaml.Marshal(got)
				assert.NoError(t, err)
				jsonbs, err := goyaml.YAMLToJSON(gotbs)
				assert.NoError(t, err)

				// uncomment following lines to update golden file
				assert.NoError(t, ioutil.WriteFile(tt.wantGolden, jsonbs, os.ModePerm))
				wantbs, err := ioutil.ReadFile(tt.wantGolden)
				assert.NoError(t, err)
				assert.Equal(t, string(wantbs), string(jsonbs))
			}
		})
	}
}
