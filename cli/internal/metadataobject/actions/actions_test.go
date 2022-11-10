package actions

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"testing"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/cliext"
	cliextension "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/cli_extension"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/mitchellh/go-homedir"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"gopkg.in/yaml.v3"
)

func TestActionConfig_Export(t *testing.T) {
	testEC := cli.NewExecutionContext()
	testEC.Version = version.NewCLIVersion(version.DevVersion)
	testEC.Logger = logrus.New()
	home, err := homedir.Dir()
	assert.NoError(t, err)
	testEC.GlobalConfigDir = filepath.Join(home, cli.GlobalConfigDirName)
	assert.NoError(t, cliext.Setup(testEC))

	type fields struct {
		MetadataDir        string
		ActionConfig       *types.ActionExecutionConfig
		serverFeatureFlags *version.ServerFeatureFlags
		cliExtensionConfig *cliextension.Config
		ensureCliExt       func() error
		cleanupCliExt      func()
		logger             *logrus.Logger
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
			"can export actions metadata",
			fields{
				MetadataDir:  "metadata",
				ActionConfig: nil,
				serverFeatureFlags: &version.ServerFeatureFlags{
					HasAction: true,
				},
				cliExtensionConfig: cliextension.NewCLIExtensionConfig(&testEC.CliExtDestinationBinPath, testEC.Logger),
				ensureCliExt: func() error {
					return cliext.Setup(testEC)
				},
				cleanupCliExt: func() {
					cliext.Cleanup(testEC)
				},
				logger: testEC.Logger,
			},
			args{
				metadata: func() map[string]yaml.Node {
					bs, err := ioutil.ReadFile("testdata/export/t1/metadata.json")
					assert.NoError(t, err)
					yamlbs, err := metadatautil.JSONToYAML(bs)
					assert.NoError(t, err)
					var v map[string]yaml.Node
					assert.NoError(t, yaml.Unmarshal(yamlbs, &v))
					return v
				}(),
			},
			map[string][]byte{
				"metadata/actions.graphql": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export/t1/want.actions.graphql")
					assert.NoError(t, err)
					return bs
				}(),
				"metadata/actions.yaml": func() []byte {
					bs, err := ioutil.ReadFile("testdata/export/t1/want.actions.yaml")
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
			a := &ActionConfig{
				MetadataDir:        tt.fields.MetadataDir,
				ActionConfig:       tt.fields.ActionConfig,
				serverFeatureFlags: tt.fields.serverFeatureFlags,
				cliExtensionConfig: tt.fields.cliExtensionConfig,
				ensureCliExt:       tt.fields.ensureCliExt,
				cleanupCliExt:      tt.fields.cleanupCliExt,
				logger:             tt.fields.logger,
			}
			got, err := a.Export(tt.args.metadata)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			for k, v := range got {
				assert.Contains(t, tt.want, k)
				// uncomment to update golden files
				// assert.NoError(t, ioutil.WriteFile(fmt.Sprintf("testdata/export/%v/want.%v", tt.id, filepath.Base(k)), v, os.ModePerm))
				assert.Equalf(t, string(tt.want[k]), string(v), "%v", k)
			}
		})
	}
}

func TestActionConfig_Build(t *testing.T) {
	testEC := cli.NewExecutionContext()
	testEC.Version = version.NewCLIVersion(version.DevVersion)
	testEC.Logger = logrus.New()
	assert.NoError(t, testEC.Prepare())
	assert.NoError(t, cliext.Setup(testEC))
	type fields struct {
		MetadataDir        string
		ActionConfig       *types.ActionExecutionConfig
		serverFeatureFlags *version.ServerFeatureFlags
		cliExtensionConfig *cliextension.Config
		ensureCliExt       func() error
		cleanupCliExt      func()
		logger             *logrus.Logger
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
			"can build metadata from project dir",
			fields{
				MetadataDir:  "testdata/build/t1/metadata",
				ActionConfig: nil,
				serverFeatureFlags: &version.ServerFeatureFlags{
					HasAction: true,
				},
				cliExtensionConfig: cliextension.NewCLIExtensionConfig(&testEC.CliExtDestinationBinPath, testEC.Logger),
				ensureCliExt: func() error {
					return cliext.Setup(testEC)
				},
				cleanupCliExt: func() {
					cliext.Cleanup(testEC)
				},
				logger: testEC.Logger,
			},
			"testdata/build/t1/want.json",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &ActionConfig{
				MetadataDir:        tt.fields.MetadataDir,
				ActionConfig:       tt.fields.ActionConfig,
				serverFeatureFlags: tt.fields.serverFeatureFlags,
				cliExtensionConfig: tt.fields.cliExtensionConfig,
				ensureCliExt:       tt.fields.ensureCliExt,
				cleanupCliExt:      tt.fields.cleanupCliExt,
				logger:             tt.fields.logger,
			}
			got, err := a.Build()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			gotbs, err := yaml.Marshal(got)
			require.NoError(t, err)
			gotjson, err := goyaml.YAMLToJSON(gotbs)
			require.NoError(t, err)
			goldenFile := fmt.Sprintf("testdata/build/%v/want.json", tt.id)
			var pretty_json bytes.Buffer
			err = json.Indent(&pretty_json, gotjson, "", " ")
			assert.NoError(t, err)
			// uncomment to update golden file
			// assert.NoError(t, ioutil.WriteFile(goldenFile, pretty_json.Bytes(), os.ModePerm))

			want, err := ioutil.ReadFile(goldenFile)
			assert.NoError(t, err)
			assert.Equal(t, string(want), pretty_json.String())
		})
	}
}
