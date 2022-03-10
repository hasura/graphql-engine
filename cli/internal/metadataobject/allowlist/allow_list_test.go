package allowlist

import (
	"bytes"
	"io/ioutil"
	"path/filepath"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"gopkg.in/yaml.v3"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/sirupsen/logrus"
)

func TestAllowListConfig_WriteDiff(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		to metadataobject.Object
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   metadataobject.ErrParsingMetadataObject
	}{
		{
			"t1",
			fields{"testdata/write_diff/t1/from", logrus.New()},
			args{
				to: func() metadataobject.Object {
					return &AllowListConfig{
						MetadataDir: "testdata/write_diff/t1/to",
						logger:      logrus.New(),
					}
				}(),
			},
			nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &AllowListConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			w := &bytes.Buffer{}
			got := a.WriteDiff(metadataobject.WriteDiffOpts{
				To:           tt.args.to,
				W:            w,
				DisableColor: true,
			})
			wantGoldenFilePath := filepath.Join("testdata/write_diff", tt.name, "want.golden")

			// uncomment to update test golden file
			//assert.NoError(t, ioutil.WriteFile(wantGoldenFilePath, w.Bytes(), os.ModePerm))

			want, err := ioutil.ReadFile(wantGoldenFilePath)
			assert.NoError(t, err)
			assert.Equal(t, string(want), w.String())
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestAllowListConfig_Export(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata map[string]yaml.Node
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    map[string][]byte
		wantErr bool
	}{
		{
			"can export allow list metadata (t1)",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export_test/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{"testdata/metadata/allow_list.yaml": []byte("- collection: allowed-queries\n")},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &AllowListConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := a.Export(tt.args.metadata)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				for k, v := range got {
					assert.Equal(t, string(tt.want[k]), string(v))
				}
			}
		})
	}
}

func TestAllowListConfig_Build(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	tests := []struct {
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
	}{
		{
			"can build metadata from file (t1)",
			fields{
				"testdata/build_test/t1",
				logrus.New(),
			},
			"testdata/build_test/t1/want.golden.json",
			false,
		},
		{
			"can build metadata from empty file (t2)",
			fields{
				"testdata/build_test/t2",
				logrus.New(),
			},
			"testdata/build_test/t2/want.golden.json",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &AllowListConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := a.Build()
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				yamlbs, err := yaml.Marshal(got)
				assert.NoError(t, err)
				// uncomment to update golden file
				jsonbs, err := goyaml.YAMLToJSON(yamlbs)
				assert.NoError(t, err)
				assert.NoError(t, err)
				// uncomment to update golden file
				//assert.NoError(t, ioutil.WriteFile(tt.wantGolden, jsonbs, os.ModePerm))

				wantbs, err := ioutil.ReadFile(tt.wantGolden)
				assert.NoError(t, err)
				assert.Equal(t, string(wantbs), string(jsonbs))
			}
		})
	}
}
