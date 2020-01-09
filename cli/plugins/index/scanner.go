package index

import (
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/hasura/graphql-engine/cli/plugins/types"
	"github.com/hasura/graphql-engine/cli/plugins/validation"
	"github.com/pkg/errors"
)

func findPluginManifestFiles(indexDir string) ([]string, error) {
	var out []string
	files, err := ioutil.ReadDir(indexDir)
	if err != nil {
		return nil, errors.Wrap(err, "failed to open index dir")
	}
	for _, file := range files {
		if file.Mode().IsRegular() && filepath.Ext(file.Name()) == paths.ManifestExtension {
			out = append(out, file.Name())
		}
	}
	return out, nil
}

// LoadPluginListFromFS will parse and retrieve all plugin files.
func LoadPluginListFromFS(indexDir string) ([]types.Plugin, error) {
	indexDir, err := filepath.EvalSymlinks(indexDir)
	if err != nil {
		return nil, err
	}

	files, err := findPluginManifestFiles(indexDir)
	if err != nil {
		return nil, errors.Wrap(err, "failed to scan plugins in index directory")
	}

	list := make([]types.Plugin, 0, len(files))
	for _, file := range files {
		pluginName := strings.TrimSuffix(file, filepath.Ext(file))
		p, err := LoadPluginByName(indexDir, pluginName)
		if err != nil {
			continue
		}
		list = append(list, p)
	}
	return list, nil
}

// LoadPluginByName loads a plugins index file by its name. When plugin
// file not found, it returns an error that can be checked with os.IsNotExist.
func LoadPluginByName(pluginsDir, pluginName string) (types.Plugin, error) {
	if !validation.IsSafePluginName(pluginName) {
		return types.Plugin{}, errors.Errorf("plugin name %q not allowed", pluginName)
	}

	return ReadPluginFromFile(filepath.Join(pluginsDir, pluginName+paths.ManifestExtension))
}

// ReadPluginFromFile loads a file from the FS. When plugin file not found, it
// returns an error that can be checked with os.IsNotExist.
func ReadPluginFromFile(path string) (types.Plugin, error) {
	f, err := os.Open(path)
	if os.IsNotExist(err) {
		return types.Plugin{}, err
	} else if err != nil {
		return types.Plugin{}, errors.Wrap(err, "failed to open index file")
	}
	return ReadPlugin(f)
}

func ReadPlugin(f io.ReadCloser) (types.Plugin, error) {
	defer f.Close()
	p, err := DecodePluginFile(f)
	if err != nil {
		return p, errors.Wrap(err, "failed to decode plugin manifest")
	}
	return p, errors.Wrap(validation.ValidatePlugin(p.Name, p), "plugin manifest validation error")
}

// DecodePluginFile tries to decodes a plugin manifest from r.
func DecodePluginFile(r io.Reader) (types.Plugin, error) {
	var plugin types.Plugin
	b, err := ioutil.ReadAll(r)
	if err != nil {
		return plugin, err
	}
	err = yaml.Unmarshal(b, &plugin)
	return plugin, err
}
