package crontriggers

import (
	"io/ioutil"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"gopkg.in/yaml.v3"
)

func TestCronTriggers_Build(t *testing.T) {
	type fields struct {
		MetadataDir        string
		logger             *logrus.Logger
		serverFeatureFlags *version.ServerFeatureFlags
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
				serverFeatureFlags: &version.ServerFeatureFlags{
					HasCronTriggers: true,
				},
			},
			"testdata/build_test/t1/want.golden.json",
			false,
			require.NoError,
		},
		{
			"t2",
			"can exclude object when not present",
			fields{
				MetadataDir: "testdata/build_test/t2/metadata",
				logger:      logrus.New(),
				serverFeatureFlags: &version.ServerFeatureFlags{
					HasCronTriggers: true,
				},
			},
			"testdata/build_test/t2/want.golden.json",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &CronTriggers{
				MetadataDir:        tt.fields.MetadataDir,
				logger:             tt.fields.logger,
				serverFeatureFlags: tt.fields.serverFeatureFlags,
			}
			got, err := c.Build()
			tt.assertErr(t, err)
			if !tt.wantErr {
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

func TestCronTriggers_Export(t *testing.T) {
	type fields struct {
		MetadataDir        string
		logger             *logrus.Logger
		serverFeatureFlags *version.ServerFeatureFlags
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
				MetadataDir:        "metadata",
				logger:             logrus.New(),
				serverFeatureFlags: &version.ServerFeatureFlags{HasCronTriggers: true},
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
				"metadata/cron_triggers.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t1/want.cron_triggers.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
			require.NoError,
		},
		{
			"t2",
			"can generate empty metadata file when no cron triggers are present",
			fields{
				MetadataDir:        "metadata",
				logger:             logrus.New(),
				serverFeatureFlags: &version.ServerFeatureFlags{HasCronTriggers: true},
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
				"metadata/cron_triggers.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export_test/t2/want.cron_triggers.yaml")
					assert.NoError(t, err)
					return bs
				}(),
			},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &CronTriggers{
				MetadataDir:        tt.fields.MetadataDir,
				logger:             tt.fields.logger,
				serverFeatureFlags: tt.fields.serverFeatureFlags,
			}
			got, err := c.Export(tt.args.metadata)
			tt.assertErr(t, err)
			if !tt.wantErr {
				for k, v := range got {
					assert.Contains(t, tt.want, k)
					// uncomment to update golden files
					//assert.NoError(t, ioutil.WriteFile(fmt.Sprintf("testdata/export_test/%v/want.%v", tt.id, filepath.Base(k)), v, os.ModePerm))
					assert.Equalf(t, string(tt.want[k]), string(v), "%v", k)
				}
			}
		})
	}
}
