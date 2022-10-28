package v3

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"
	"gopkg.in/yaml.v3"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
)

// TODO: move this to testutil
func editEndpointInConfig(t *testing.T, configFilePath, endpoint string) {
	var config cli.Config
	b, err := ioutil.ReadFile(configFilePath)
	require.NoError(t, err)

	err = yaml.Unmarshal(b, &config)
	require.NoError(t, err)

	config.Endpoint = endpoint

	b, err = yaml.Marshal(&config)
	require.NoError(t, err)

	err = ioutil.WriteFile(configFilePath, b, 0655)
	require.NoError(t, err)

}

func TestInitCmd(t *testing.T, ec *cli.ExecutionContext, initDir, hasuraPort string) {
	tt := []struct {
		name string
		opts *commands.InitOptions
		err  error
	}{
		{"only-init-dir", &commands.InitOptions{
			EC:          ec,
			Version:     cli.V3,
			AdminSecret: os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
			InitDir:     initDir,
		}, nil},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.opts.Run()
			if err != tc.err {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			}

			editEndpointInConfig(t, filepath.Join(initDir, "config.yaml"), fmt.Sprintf("http://localhost:%s", hasuraPort))
		})
	}
}
