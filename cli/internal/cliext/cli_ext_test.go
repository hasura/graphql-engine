package cliext

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2"
)

func TestSetupUsesCliExtPathInPlace(t *testing.T) {
	binPath := filepath.Join(t.TempDir(), "cli-ext")
	require.NoError(t, os.WriteFile(binPath, []byte("binary"), 0o755))

	ec := cli.NewExecutionContext()
	ec.Logger = logrus.New()
	ec.GlobalConfigDir = t.TempDir()
	ec.CliExtSourceBinPath = binPath

	require.NoError(t, Setup(ec))
	assert.Equal(t, binPath, ec.CliExtDestinationBinPath)
	assert.Empty(t, ec.CliExtDestinationDir)

	_, err := os.Stat(filepath.Join(ec.GlobalConfigDir, "cli-ext"))
	assert.True(t, os.IsNotExist(err), "nothing should be unpacked into the global config dir")

	Cleanup(ec)
	_, err = os.Stat(binPath)
	assert.NoError(t, err, "Cleanup must not remove the binary given with --cli-ext-path")
}

func TestSetupRejectsMissingOrDirectoryCliExtPath(t *testing.T) {
	dir := t.TempDir()
	for _, path := range []string{filepath.Join(dir, "missing"), dir} {
		ec := cli.NewExecutionContext()
		ec.Logger = logrus.New()
		ec.GlobalConfigDir = t.TempDir()
		ec.CliExtSourceBinPath = path

		assert.Error(t, Setup(ec), path)
		assert.Empty(t, ec.CliExtDestinationBinPath, path)
	}
}
