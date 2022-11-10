package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	stderrors "errors"
	"fmt"
	"io/fs"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"

	"github.com/Masterminds/semver"

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/plugins/paths"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/sirupsen/logrus"
)

var (
	ErrIsAlreadyInstalled  = fmt.Errorf("can't install, plugin is already installed")
	ErrIsNotInstalled      = fmt.Errorf("plugin is not installed")
	ErrIsAlreadyUpgraded   = fmt.Errorf("can't upgrade, the newest version is already installed")
	ErrVersionNotAvailable = fmt.Errorf("plugin version is not available")
)

// IndexBranchRef - branch to be used for index
var IndexBranchRef = "master"

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

// FetchOpts - options available during fetching plugin manifest
type FetchOpts struct {
	ManifestFile string

	Version *semver.Version
}

func New(base string) *Config {
	p := paths.NewPaths(base)
	return &Config{
		Paths: p,
		Repo:  util.NewGitUtil(indexURI, p.IndexPath(), IndexBranchRef),
	}
}

// Prepare makes sure that the plugins directory is initialized
func (c *Config) Prepare() error {
	var op errors.Op = "plugins.Config.Prepare"
	err := ensureDirs(c.Paths.BasePath(),
		c.Paths.DownloadPath(),
		c.Paths.InstallPath(),
		c.Paths.BinPath(),
		c.Paths.InstallReceiptsPath(),
	)
	if err != nil {
		return errors.E(op, fmt.Errorf("unable to create plugin directories: %w", err))
	}
	return nil
}

// ListInstalledPlugins returns a list of all install plugins in a
// name:version format based on the install receipts at the specified dir.
func (c *Config) ListInstalledPlugins() (map[string]string, error) {
	var op errors.Op = "plugins.Config.ListInstalledPlugins"
	receiptsDir := c.Paths.InstallReceiptsPath()
	matches, err := filepath.Glob(filepath.Join(receiptsDir, "*"+paths.ManifestExtension))
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("failed to grab receipts directory (%s) for manifests: %w", receiptsDir, err))
	}
	installed := make(map[string]string)
	for _, m := range matches {
		r, err := c.LoadManifest(m)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("failed to parse plugin install receipt %s: %w", m, err))
		}
		installed[r.Name] = r.Version
	}
	return installed, nil
}

func (c *Config) ListPlugins() (Plugins, error) {
	var op errors.Op = "plugins.Config.ListPlugins"
	plugins, err := c.LoadPluginListFromFS(c.Paths.IndexPluginsPath())
	if err != nil {
		return plugins, errors.E(op, err)
	}
	return plugins, nil
}

func (c *Config) GetPlugin(pluginName string, opts FetchOpts) (Plugin, error) {
	var op errors.Op = "plugins.Config.GetPlugin"
	var plugin Plugin
	var err error
	if opts.ManifestFile == "" {
		// Load the plugin index by name
		ps, err := c.LoadPluginByName(pluginName)
		if err != nil {
			if stderrors.Is(err, fs.ErrNotExist) {
				return plugin, errors.E(op, fmt.Errorf("plugin %q does not exist in the plugin index", pluginName))
			}
			return plugin, errors.E(op, fmt.Errorf("failed to load plugin %q from the index: %w", pluginName, err))
		}

		// Load the installed manifest
		pluginReceipt, err := c.LoadManifest(c.Paths.PluginInstallReceiptPath(pluginName))
		if err != nil && !stderrors.Is(err, fs.ErrNotExist) {
			return plugin, errors.E(op, fmt.Errorf("failed to look up plugin receipt: %w", err))
		}

		if opts.Version != nil {
			if pluginReceipt.Version == opts.Version.Original() {
				return plugin, errors.E(op, ErrIsAlreadyInstalled)
			}
			// check if version is available
			ver := ps.Index.Search(opts.Version)
			if ver != nil {
				plugin = ps.Versions[ver]
			} else {
				return plugin, errors.E(op, ErrVersionNotAvailable)
			}
		} else {
			if err == nil {
				return plugin, errors.E(op, ErrIsAlreadyInstalled)
			}
			// get the latest version
			latestVersion := ps.Index[len(ps.Index)-1]
			plugin = ps.Versions[latestVersion]
		}
	} else {
		plugin, err = c.ReadPluginFromFile(opts.ManifestFile)
		if err != nil {
			return plugin, errors.E(op, fmt.Errorf("failed to load plugin manifest from file: %w", err))
		}
		if plugin.Name != pluginName {
			return plugin, errors.E(op, fmt.Errorf("plugin name %s doesn't match with plugin in the manifest file %s", pluginName, opts.ManifestFile))
		}
	}
	return plugin, nil
}

func (c *Config) Install(plugin Plugin) error {
	var op errors.Op = "plugins.Config.Install"
	// Find available installation platform
	platform, ok, err := MatchPlatform(plugin.Platforms)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed trying to find a matching platform in plugin spec: %w", err))
	}
	if !ok {
		return errors.E(op, fmt.Errorf("plugin %q does not offer installation for this platform", plugin.Name))
	}
	if err := c.installPlugin(plugin, platform); err != nil {
		return errors.E(op, fmt.Errorf("install failed: %w", err))
	}
	err = c.StoreManifest(plugin, c.Paths.PluginInstallReceiptPath(plugin.Name))
	if err != nil {
		return errors.E(op, fmt.Errorf("installation receipt could not be stored, uninstall may fail: %w", err))
	}
	return nil
}

// Uninstall will uninstall a plugin.
func (c *Config) Uninstall(name string) error {
	var op errors.Op = "plugins.Config.Uninstall"
	if _, err := c.LoadManifest(c.Paths.PluginInstallReceiptPath(name)); err != nil {
		if stderrors.Is(err, fs.ErrNotExist) {
			return errors.E(op, ErrIsNotInstalled)
		}
		return errors.E(op, fmt.Errorf("failed to look up install receipt for plugin %q: %w", name, err))
	}

	symlinkPath := filepath.Join(c.Paths.BinPath(), PluginNameToBin(name, IsWindows()))
	if err := removeLink(symlinkPath); err != nil {
		return errors.E(op, fmt.Errorf("could not uninstall symlink of plugin: %w", err))
	}

	pluginInstallPath := c.Paths.PluginInstallPath(name)
	if err := os.RemoveAll(pluginInstallPath); err != nil {
		return errors.E(op, fmt.Errorf("could not remove plugin directory %q: %w", pluginInstallPath, err))
	}
	pluginReceiptPath := c.Paths.PluginInstallReceiptPath(name)
	err := os.Remove(pluginReceiptPath)
	if err != nil {
		return errors.E(op, fmt.Errorf("could not remove plugin receipt %q: %w", pluginReceiptPath, err))
	}
	return nil
}

func (c *Config) installPlugin(plugin Plugin, platform Platform) error {
	var op errors.Op = "plugins.Config.installPlugin"
	downloadStagingDir := filepath.Join(c.Paths.DownloadPath(), plugin.Name)
	installDir := c.Paths.PluginVersionInstallPath(plugin.Name, plugin.Version)
	binDir := c.Paths.BinPath()

	// check if install dir already exists
	_, err := os.Stat(installDir)
	if err != nil {
		// Download and extract
		if err := os.MkdirAll(downloadStagingDir, 0755); err != nil {
			return errors.E(op, fmt.Errorf("could not create staging dir %q: %w", downloadStagingDir, err))
		}
		defer func() {
			c.Logger.Debugf("Deleting the download staging directory %s", downloadStagingDir)
			if err := os.RemoveAll(downloadStagingDir); err != nil {
				c.Logger.Debugf("failed to clean up download staging directory: %s", err)
			}
		}()

		if err := downloadAndExtract(downloadStagingDir, platform.URI, platform.Sha256); err != nil {
			return errors.E(op, fmt.Errorf("failed to unpack into staging dir: %w", err))
		}

		if err := moveToInstallDir(downloadStagingDir, installDir, platform.Files); err != nil {
			return errors.E(op, fmt.Errorf("failed while moving files to the installation directory: %w", err))
		}
	}

	subPathAbs, err := filepath.Abs(installDir)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to get the absolute fullPath of %q: %w", installDir, err))
	}
	fullPath := filepath.Join(installDir, filepath.FromSlash(platform.Bin))
	pathAbs, err := filepath.Abs(fullPath)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to get the absolute fullPath of %q: %w", fullPath, err))
	}
	if _, ok := IsSubPath(subPathAbs, pathAbs); !ok {
		if err != nil {
			return errors.E(op, fmt.Errorf("the fullPath %q does not extend the sub-fullPath %q: %w", fullPath, installDir, err))
		}
		return nil
	}
	err = createOrUpdateLink(binDir, fullPath, plugin.Name)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to link installed plugin: %w", err))
	}
	return nil
}

// Upgrade will reinstall and delete the old plugin. The operation tries
// to not get the plugin dir in a bad state if it fails during the process.
func (c *Config) Upgrade(pluginName string, version *semver.Version) (Plugin, error) {
	var op errors.Op = "plugins.Config.Upgrade"
	ps, err := c.LoadPluginByName(pluginName)
	if err != nil {
		if stderrors.Is(err, fs.ErrNotExist) {
			return Plugin{}, errors.E(op, fmt.Errorf("plugin %q does not exist in the plugin index", pluginName))
		}
		return Plugin{}, errors.E(op, fmt.Errorf("failed to load the plugin manifest for plugin %s: %w", pluginName, err))
	}

	// get the latest version
	var plugin Plugin
	if version != nil {
		ver := ps.Index.Search(version)
		if ver != nil {
			plugin = ps.Versions[ver]
		} else {
			return Plugin{}, errors.E(op, ErrVersionNotAvailable)
		}
	} else {
		latestVersion := ps.Index[len(ps.Index)-1]
		plugin = ps.Versions[latestVersion]
	}

	installReceipt, err := c.LoadManifest(c.Paths.PluginInstallReceiptPath(plugin.Name))
	if err != nil {
		return plugin, errors.E(op, fmt.Errorf("failed to load install receipt for plugin %q: %w", plugin.Name, err))
	}

	if installReceipt.ParsedVersion == nil {
		c.Logger.Debugf("failed to parse installed plugin version (%q) as a semver value", installReceipt.ParsedVersion)
		c.Logger.Debugf("assuming installed plugin %s as a dev version and force upgrade", plugin.Name)
	}

	// Find available installation platform
	platform, ok, err := MatchPlatform(plugin.Platforms)
	if err != nil {
		return plugin, errors.E(op, fmt.Errorf("failed trying to find a matching platform in plugin spec: %w", err))
	}
	if !ok {
		return plugin, errors.E(op, fmt.Errorf("plugin %q does not offer installation for this platform (%s)",
			plugin.Name, fmt.Sprintf("%s-%s", runtime.GOOS, runtime.GOARCH)))
	}

	// See if it's a newer version
	if installReceipt.ParsedVersion != nil {
		if !installReceipt.ParsedVersion.LessThan(plugin.ParsedVersion) || installReceipt.ParsedVersion.Equal(plugin.ParsedVersion) {
			return plugin, errors.E(op, ErrIsAlreadyUpgraded)
		}
	}

	if err = c.StoreManifest(plugin, c.Paths.PluginInstallReceiptPath(plugin.Name)); err != nil {
		return plugin, errors.E(op, fmt.Errorf("installation receipt could not be stored, uninstall may fail: %w", err))
	}

	// Re-Install
	if err := c.installPlugin(plugin, platform); err != nil {
		return plugin, errors.E(op, fmt.Errorf("failed to install new version: %w", err))
	}

	// Clean old installations
	err = os.RemoveAll(c.Paths.PluginVersionInstallPath(plugin.Name, installReceipt.Version))
	if err != nil {
		return plugin, errors.E(op, err)
	}
	return plugin, nil
}

func (c *Config) LoadManifest(path string) (Plugin, error) {
	var op errors.Op = "plugins.Config.LoadManifest"
	plugin, err := c.ReadPluginFromFile(path)
	if err != nil {
		return plugin, errors.E(op, err)
	}
	return plugin, nil
}

func (c *Config) StoreManifest(plugin Plugin, dest string) error {
	var op errors.Op = "plugins.Config.StoreManifest"
	yamlBytes, err := yaml.Marshal(plugin)
	if err != nil {
		return errors.E(op, fmt.Errorf("convert to yaml: %w", err))
	}
	err = ioutil.WriteFile(dest, yamlBytes, 0644)
	if err != nil {
		return errors.E(op, fmt.Errorf("write plugin receipt %q: %w", dest, err))
	}
	return nil

}
