package v2

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/stretchr/testify/require"
	"go.yaml.in/yaml/v3"
)

// TODO: move this to testutil.
func editEndpointInConfig(t *testing.T, configFilePath, endpoint string) {
	t.Helper()

	var config cli.Config

	b, err := os.ReadFile(configFilePath)
	require.NoError(t, err)

	err = yaml.Unmarshal(b, &config)
	require.NoError(t, err)

	config.Endpoint = endpoint

	b, err = yaml.Marshal(&config)
	require.NoError(t, err)

	err = os.WriteFile(configFilePath, b, 0o655)
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
			Version:     cli.V2,
			AdminSecret: os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
			InitDir:     initDir,
		}, nil},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.opts.Run()
			if !errors.Is(err, tc.err) {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			}

			editEndpointInConfig(
				t,
				filepath.Join(initDir, "config.yaml"),
				"http://localhost:"+hasuraPort,
			)
			// TODO: (shahidhk) need to verify the contents of the spec generated
		})
	}
}
