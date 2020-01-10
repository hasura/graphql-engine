package installation

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/plugins/download"
	"github.com/hasura/graphql-engine/cli/plugins/index"
	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/hasura/graphql-engine/cli/plugins/pathutil"
	"github.com/hasura/graphql-engine/cli/plugins/types"
	"github.com/pkg/errors"
)

type installOperation struct {
	pluginName string
	platform   types.Platform

	downloadStagingDir string
	installDir         string
	binDir             string
}

var (
	ErrIsAlreadyInstalled = errors.New("can't install, the newest version is already installed")
	ErrIsNotInstalled     = errors.New("plugin is not installed")
	ErrIsAlreadyUpgraded  = errors.New("can't upgrade, the newest version is already installed")
)

// ListInstalledPlugins returns a list of all install plugins in a
// name:version format based on the install receipts at the specified dir.
func ListInstalledPlugins(receiptsDir string) (map[string]string, error) {
	matches, err := filepath.Glob(filepath.Join(receiptsDir, "*"+paths.ManifestExtension))
	if err != nil {
		return nil, errors.Wrapf(err, "failed to grab receipts directory (%s) for manifests", receiptsDir)
	}
	installed := make(map[string]string)
	for _, m := range matches {
		r, err := Load(m)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to parse plugin install receipt %s", m)
		}
		installed[r.Name] = r.Version
	}
	return installed, nil
}

// Install will download and install a plugin. The operation tries
// to not get the plugin dir in a bad state if it fails during the process.
func Install(p paths.Paths, plugin types.Plugin) error {
	_, err := Load(p.PluginInstallReceiptPath(plugin.Name))
	if err == nil {
		return ErrIsAlreadyInstalled
	} else if !os.IsNotExist(err) {
		return errors.Wrap(err, "failed to look up plugin receipt")
	}

	// Find available installation candidate
	candidate, ok, err := GetMatchingPlatform(plugin.Platforms)
	if err != nil {
		return errors.Wrap(err, "failed trying to find a matching platform in plugin spec")
	}
	if !ok {
		return errors.Errorf("plugin %q does not offer installation for this platform", plugin.Name)
	}

	if err := install(installOperation{
		pluginName: plugin.Name,
		platform:   candidate,

		downloadStagingDir: filepath.Join(p.DownloadPath(), plugin.Name),
		binDir:             p.BinPath(),
		installDir:         p.PluginVersionInstallPath(plugin.Name, plugin.Version),
	}); err != nil {
		return errors.Wrap(err, "install failed")
	}
	err = Store(plugin, p.PluginInstallReceiptPath(plugin.Name))
	return errors.Wrap(err, "installation receipt could not be stored, uninstall may fail")
}

func install(op installOperation) error {
	// Download and extract
	if err := os.MkdirAll(op.downloadStagingDir, 0755); err != nil {
		return errors.Wrapf(err, "could not create staging dir %q", op.downloadStagingDir)
	}
	defer func() {
		if err := os.RemoveAll(op.downloadStagingDir); err != nil {
			// log warning message
		}
	}()
	if err := downloadAndExtract(op.downloadStagingDir, op.platform.URI, op.platform.Sha256); err != nil {
		return errors.Wrap(err, "failed to unpack into staging dir")
	}

	if err := moveToInstallDir(op.downloadStagingDir, op.installDir, op.platform.Files); err != nil {
		return errors.Wrap(err, "failed while moving files to the installation directory")
	}

	subPathAbs, err := filepath.Abs(op.installDir)
	if err != nil {
		return errors.Wrapf(err, "failed to get the absolute fullPath of %q", op.installDir)
	}
	fullPath := filepath.Join(op.installDir, filepath.FromSlash(op.platform.Bin))
	pathAbs, err := filepath.Abs(fullPath)
	if err != nil {
		return errors.Wrapf(err, "failed to get the absolute fullPath of %q", fullPath)
	}
	if _, ok := pathutil.IsSubPath(subPathAbs, pathAbs); !ok {
		return errors.Wrapf(err, "the fullPath %q does not extend the sub-fullPath %q", fullPath, op.installDir)
	}
	err = createOrUpdateLink(op.binDir, fullPath, op.pluginName)
	return errors.Wrap(err, "failed to link installed plugin")
}

// downloadAndExtract downloads the specified archive uri (or uses the provided overrideFile, if a non-empty value)
// while validating its checksum with the provided sha256sum, and extracts its contents to extractDir that must be.
// created.
func downloadAndExtract(extractDir, uri, sha256sum string) error {
	var fetcher download.Fetcher = download.HTTPFetcher{}
	verifier := download.NewSha256Verifier(sha256sum)
	err := download.NewDownloader(verifier, fetcher).Get(uri, extractDir)
	return errors.Wrap(err, "failed to unpack the plugin archive")
}

// Uninstall will uninstall a plugin.
func Uninstall(p paths.Paths, name string) error {
	if _, err := Load(p.PluginInstallReceiptPath(name)); err != nil {
		if os.IsNotExist(err) {
			return ErrIsNotInstalled
		}
		return errors.Wrapf(err, "failed to look up install receipt for plugin %q", name)
	}

	symlinkPath := filepath.Join(p.BinPath(), pluginNameToBin(name, IsWindows()))
	if err := removeLink(symlinkPath); err != nil {
		return errors.Wrap(err, "could not uninstall symlink of plugin")
	}

	pluginInstallPath := p.PluginInstallPath(name)
	if err := os.RemoveAll(pluginInstallPath); err != nil {
		return errors.Wrapf(err, "could not remove plugin directory %q", pluginInstallPath)
	}
	pluginReceiptPath := p.PluginInstallReceiptPath(name)
	err := os.Remove(pluginReceiptPath)
	return errors.Wrapf(err, "could not remove plugin receipt %q", pluginReceiptPath)
}

func createOrUpdateLink(binDir, binary, plugin string) error {
	dst := filepath.Join(binDir, pluginNameToBin(plugin, IsWindows()))

	if err := removeLink(dst); err != nil {
		return errors.Wrap(err, "failed to remove old symlink")
	}
	if _, err := os.Stat(binary); os.IsNotExist(err) {
		return errors.Wrapf(err, "can't create symbolic link, source binary (%q) cannot be found in extracted archive", binary)
	}

	// Create new
	if err := os.Symlink(binary, dst); err != nil {
		return errors.Wrapf(err, "failed to create a symlink from %q to %q", binary, dst)
	}
	return nil
}

// removeLink removes a symlink reference if exists.
func removeLink(path string) error {
	fi, err := os.Lstat(path)
	if os.IsNotExist(err) {
		return nil
	} else if err != nil {
		return errors.Wrapf(err, "failed to read the symlink in %q", path)
	}

	if fi.Mode()&os.ModeSymlink == 0 {
		return errors.Errorf("file %q is not a symlink (mode=%s)", path, fi.Mode())
	}
	if err := os.Remove(path); err != nil {
		return errors.Wrapf(err, "failed to remove the symlink in %q", path)
	}
	return nil
}

// IsWindows sees runtime.GOOS to find out if current execution mode is win32.
func IsWindows() bool {
	goos := runtime.GOOS
	return goos == "windows"
}

// pluginNameToBin creates the name of the symlink file for the plugin name.
// It converts dashes to underscores.
func pluginNameToBin(name string, isWindows bool) string {
	name = strings.ReplaceAll(name, "-", "_")
	name = "hasura-" + name
	if isWindows {
		name += ".exe"
	}
	return name
}

// Store saves the given plugin at the destination.
// The caller has to ensure that the destination directory exists.
func Store(plugin types.Plugin, dest string) error {
	yamlBytes, err := yaml.Marshal(plugin)
	if err != nil {
		return errors.Wrapf(err, "convert to yaml")
	}

	err = ioutil.WriteFile(dest, yamlBytes, 0644)
	return errors.Wrapf(err, "write plugin receipt %q", dest)
}

// Load reads the plugin receipt at the specified destination.
// If not found, it returns os.IsNotExist error.
func Load(path string) (types.Plugin, error) {
	return index.ReadPluginFromFile(path)
}
