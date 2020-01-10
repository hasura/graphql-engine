package installation

import (
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/pkg/errors"
)

// GetBinaryPath returns the binary path for a plugin
func GetBinaryPath(p paths.Paths, name string) (string, error) {
	if _, err := Load(p.PluginInstallReceiptPath(name)); err != nil {
		if os.IsNotExist(err) {
			return "", ErrIsNotInstalled
		}
		return "", errors.Wrapf(err, "failed to look up install receipt for plugin %q", name)
	}
	symlinkPath := filepath.Join(p.BinPath(), pluginNameToBin(name, IsWindows()))
	return symlinkPath, nil
}
