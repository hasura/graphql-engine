package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"

	"github.com/Masterminds/semver"
	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

var (
	ErrIsAlreadyInstalled = errors.New("can't install, the newest version is already installed")
	ErrIsNotInstalled     = errors.New("plugin is not installed")
	ErrIsAlreadyUpgraded  = errors.New("can't upgrade, the newest version is already installed")
)

const (
	indexURI string = "https://github.com/hasura/cli-plugins-index.git"
)

// Config defines the required
type Config struct {
	// Paths contains all important environment paths
	Paths paths.Paths

	// Repo defines the git object required to maintain the plugin index
	Repo *util.GitUtil

	Logger *logrus.Logger
}

func New(base string) *Config {
	p := paths.NewPaths(base)
	return &Config{
		Paths: p,
		Repo:  util.NewGitUtil(indexURI, p.IndexPath(), ""),
	}
}

// Prepare makes sure that the plugins directory is initialized
func (c *Config) Prepare() error {
	return errors.Wrap(ensureDirs(c.Paths.BasePath(),
		c.Paths.DownloadPath(),
		c.Paths.InstallPath(),
		c.Paths.BinPath(),
		c.Paths.InstallReceiptsPath(),
	), "unable to create plugin directories")
}

// ListInstalledPlugins returns a list of all install plugins in a
// name:version format based on the install receipts at the specified dir.
func (c *Config) ListInstalledPlugins() (map[string]string, error) {
	receiptsDir := c.Paths.InstallReceiptsPath()
	matches, err := filepath.Glob(filepath.Join(receiptsDir, "*"+paths.ManifestExtension))
	if err != nil {
		return nil, errors.Wrapf(err, "failed to grab receipts directory (%s) for manifests", receiptsDir)
	}
	installed := make(map[string]string)
	for _, m := range matches {
		r, err := c.LoadManifest(m)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to parse plugin install receipt %s", m)
		}
		installed[r.Name] = r.Version
	}
	return installed, nil
}

func (c *Config) ListPlugins() ([]Plugin, error) {
	return c.LoadPluginListFromFS(c.Paths.IndexPluginsPath())
}

func (c *Config) Install(pluginName string, mainfestFile string) error {
	var plugin Plugin
	var err error
	if mainfestFile == "" {
		// Load the plugin index by name
		plugin, err = c.LoadPluginByName(c.Paths.IndexPluginsPath(), pluginName)
		if err != nil {
			if os.IsNotExist(err) {
				return errors.Errorf("plugin %q does not exist in the plugin index", pluginName)
			}
			return errors.Wrapf(err, "failed to load plugin %q from the index", pluginName)
		}

		// Load the installed manifest
		_, err = c.LoadManifest(c.Paths.PluginInstallReceiptPath(plugin.Name))
		if err == nil {
			return ErrIsAlreadyInstalled
		} else if !os.IsNotExist(err) {
			return errors.Wrap(err, "failed to look up plugin receipt")
		}
	} else {
		plugin, err = c.ReadPluginFromFile(mainfestFile)
		if err != nil {
			return errors.Wrap(err, "failed to load plugin manifest from file")
		}
		if plugin.Name != pluginName {
			return fmt.Errorf("plugin name %s doesn't match with plugin in the manifest file %s", pluginName, mainfestFile)
		}
	}

	// Find available installation platform
	platform, ok, err := MatchPlatform(plugin.Platforms)
	if err != nil {
		return errors.Wrap(err, "failed trying to find a matching platform in plugin spec")
	}
	if !ok {
		return errors.Errorf("plugin %q does not offer installation for this platform", plugin.Name)
	}

	if err := c.installPlugin(plugin, platform); err != nil {
		return errors.Wrap(err, "install failed")
	}
	err = c.StoreManifest(plugin, c.Paths.PluginInstallReceiptPath(plugin.Name))
	return errors.Wrap(err, "installation receipt could not be stored, uninstall may fail")
}

// Uninstall will uninstall a plugin.
func (c *Config) Uninstall(name string) error {
	if _, err := c.LoadManifest(c.Paths.PluginInstallReceiptPath(name)); err != nil {
		if os.IsNotExist(err) {
			return ErrIsNotInstalled
		}
		return errors.Wrapf(err, "failed to look up install receipt for plugin %q", name)
	}

	symlinkPath := filepath.Join(c.Paths.BinPath(), pluginNameToBin(name, IsWindows()))
	if err := removeLink(symlinkPath); err != nil {
		return errors.Wrap(err, "could not uninstall symlink of plugin")
	}

	pluginInstallPath := c.Paths.PluginInstallPath(name)
	if err := os.RemoveAll(pluginInstallPath); err != nil {
		return errors.Wrapf(err, "could not remove plugin directory %q", pluginInstallPath)
	}
	pluginReceiptPath := c.Paths.PluginInstallReceiptPath(name)
	err := os.Remove(pluginReceiptPath)
	return errors.Wrapf(err, "could not remove plugin receipt %q", pluginReceiptPath)
}

func (c *Config) installPlugin(plugin Plugin, platform Platform) error {
	downloadStagingDir := filepath.Join(c.Paths.DownloadPath(), plugin.Name)
	installDir := c.Paths.PluginVersionInstallPath(plugin.Name, plugin.Version)
	binDir := c.Paths.BinPath()

	// Download and extract
	if err := os.MkdirAll(downloadStagingDir, 0755); err != nil {
		return errors.Wrapf(err, "could not create staging dir %q", downloadStagingDir)
	}
	defer func() {
		c.Logger.Debugf("Deleting the download staging directory %s", downloadStagingDir)
		if err := os.RemoveAll(downloadStagingDir); err != nil {
			c.Logger.Debugf("failed to clean up download staging directory: %s", err)
		}
	}()

	if err := downloadAndExtract(downloadStagingDir, platform.URI, platform.Sha256); err != nil {
		return errors.Wrap(err, "failed to unpack into staging dir")
	}

	if err := moveToInstallDir(downloadStagingDir, installDir, platform.Files); err != nil {
		return errors.Wrap(err, "failed while moving files to the installation directory")
	}

	subPathAbs, err := filepath.Abs(installDir)
	if err != nil {
		return errors.Wrapf(err, "failed to get the absolute fullPath of %q", installDir)
	}
	fullPath := filepath.Join(installDir, filepath.FromSlash(platform.Bin))
	pathAbs, err := filepath.Abs(fullPath)
	if err != nil {
		return errors.Wrapf(err, "failed to get the absolute fullPath of %q", fullPath)
	}
	if _, ok := IsSubPath(subPathAbs, pathAbs); !ok {
		return errors.Wrapf(err, "the fullPath %q does not extend the sub-fullPath %q", fullPath, installDir)
	}
	err = createOrUpdateLink(binDir, fullPath, plugin.Name)
	return errors.Wrap(err, "failed to link installed plugin")
}

// Upgrade will reinstall and delete the old plugin. The operation tries
// to not get the plugin dir in a bad state if it fails during the process.
func (c *Config) Upgrade(pluginName string) (Plugin, error) {
	plugin, err := c.LoadPluginByName(c.Paths.IndexPluginsPath(), pluginName)
	if err != nil {
		if os.IsNotExist(err) {
			return Plugin{}, errors.Errorf("plugin %q does not exist in the plugin index", pluginName)
		}
		return Plugin{}, errors.Wrapf(err, "failed to load the plugin manifest for plugin %s", pluginName)
	}
	installReceipt, err := c.LoadManifest(c.Paths.PluginInstallReceiptPath(plugin.Name))
	if err != nil {
		return plugin, errors.Wrapf(err, "failed to load install receipt for plugin %q", plugin.Name)
	}

	curVersion := installReceipt.Version
	curv, err := semver.NewVersion(curVersion)
	if err != nil {
		c.Logger.Debugf("failed to parse installed plugin version (%q) as a semver value", curVersion)
		c.Logger.Debugf("assuming installed plugin %s as a dev version and force upgrade", plugin.Name)
	}

	// Find available installation platform
	platform, ok, err := MatchPlatform(plugin.Platforms)
	if err != nil {
		return plugin, errors.Wrap(err, "failed trying to find a matching platform in plugin spec")
	}
	if !ok {
		return plugin, errors.Errorf("plugin %q does not offer installation for this platform (%s)",
			plugin.Name, fmt.Sprintf("%s-%s", runtime.GOOS, runtime.GOARCH))
	}

	newVersion := plugin.Version
	newv, err := semver.NewVersion(newVersion)
	if err != nil {
		return plugin, errors.Wrapf(err, "failed to parse candidate version spec (%q)", newVersion)
	}

	// See if it's a newer version
	if curv != nil {
		if !curv.LessThan(newv) {
			return plugin, ErrIsAlreadyUpgraded
		}
	}

	if err = c.StoreManifest(plugin, c.Paths.PluginInstallReceiptPath(plugin.Name)); err != nil {
		return plugin, errors.Wrap(err, "installation receipt could not be stored, uninstall may fail")
	}

	// Re-Install
	if err := c.installPlugin(plugin, platform); err != nil {
		return plugin, errors.Wrap(err, "failed to install new version")
	}

	// Clean old installations
	return plugin, os.RemoveAll(c.Paths.PluginVersionInstallPath(plugin.Name, curVersion))
}

func (c *Config) LoadManifest(path string) (Plugin, error) {
	return c.ReadPluginFromFile(path)
}

func (c *Config) StoreManifest(plugin Plugin, dest string) error {
	yamlBytes, err := yaml.Marshal(plugin)
	if err != nil {
		return errors.Wrapf(err, "convert to yaml")
	}
	err = ioutil.WriteFile(dest, yamlBytes, 0644)
	return errors.Wrapf(err, "write plugin receipt %q", dest)
}
