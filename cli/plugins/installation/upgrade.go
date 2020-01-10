package installation

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/Masterminds/semver"
	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/hasura/graphql-engine/cli/plugins/types"
	"github.com/pkg/errors"
)

// Upgrade will reinstall and delete the old plugin. The operation tries
// to not get the plugin dir in a bad state if it fails during the process.
func Upgrade(p paths.Paths, plugin types.Plugin) error {
	installReceipt, err := Load(p.PluginInstallReceiptPath(plugin.Name))
	if err != nil {
		return errors.Wrapf(err, "failed to load install receipt for plugin %q", plugin.Name)
	}

	curVersion := installReceipt.Version
	curv, err := semver.NewVersion(curVersion)
	if err != nil {
		return errors.Wrapf(err, "failed to parse installed plugin version (%q) as a semver value", curVersion)
	}

	// Find available installation candidate
	candidate, ok, err := GetMatchingPlatform(plugin.Platforms)
	if err != nil {
		return errors.Wrap(err, "failed trying to find a matching platform in plugin spec")
	}
	if !ok {
		return errors.Errorf("plugin %q does not offer installation for this platform (%s)",
			plugin.Name, fmt.Sprintf("%s-%s", runtime.GOOS, runtime.GOARCH))
	}

	newVersion := plugin.Version
	newv, err := semver.NewVersion(newVersion)
	if err != nil {
		return errors.Wrapf(err, "failed to parse candidate version spec (%q)", newVersion)
	}

	// See if it's a newer version
	if !curv.LessThan(newv) {
		return ErrIsAlreadyUpgraded
	}

	if err = Store(plugin, p.PluginInstallReceiptPath(plugin.Name)); err != nil {
		return errors.Wrap(err, "installation receipt could not be stored, uninstall may fail")
	}

	// Re-Install
	if err := install(installOperation{
		pluginName:         plugin.Name,
		platform:           candidate,
		downloadStagingDir: filepath.Join(p.DownloadPath(), plugin.Name),
		installDir:         p.PluginVersionInstallPath(plugin.Name, newVersion),
		binDir:             p.BinPath(),
	}); err != nil {
		return errors.Wrap(err, "failed to install new version")
	}

	// Clean old installations
	return cleanupInstallation(p, plugin, curVersion)
}

// cleanupInstallation will remove a plugin directly.
func cleanupInstallation(p paths.Paths, plugin types.Plugin, oldVersion string) error {
	return os.RemoveAll(p.PluginVersionInstallPath(plugin.Name, oldVersion))
}
