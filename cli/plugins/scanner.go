package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/pkg/errors"
)

func (c *Config) findPluginManifestFiles(indexDir string) ([]string, error) {
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
func (c *Config) LoadPluginListFromFS(indexDir string) ([]Plugin, error) {
	indexDir, err := filepath.EvalSymlinks(indexDir)
	if err != nil {
		return nil, err
	}

	files, err := c.findPluginManifestFiles(indexDir)
	if err != nil {
		return nil, errors.Wrap(err, "failed to scan plugins in index directory")
	}

	list := make([]Plugin, 0, len(files))
	for _, file := range files {
		pluginName := strings.TrimSuffix(file, filepath.Ext(file))
		p, err := c.LoadPluginByName(indexDir, pluginName)
		if err != nil {
			continue
		}
		list = append(list, p)
	}
	return list, nil
}

// LoadPluginByName loads a plugins index file by its name. When plugin
// file not found, it returns an error that can be checked with os.IsNotExist.
func (c *Config) LoadPluginByName(pluginsDir, pluginName string) (Plugin, error) {
	if !IsSafePluginName(pluginName) {
		return Plugin{}, errors.Errorf("plugin name %q not allowed", pluginName)
	}

	return c.ReadPluginFromFile(filepath.Join(pluginsDir, pluginName+paths.ManifestExtension))
}

// ReadPluginFromFile loads a file from the FS. When plugin file not found, it
// returns an error that can be checked with os.IsNotExist.
func (c *Config) ReadPluginFromFile(path string) (Plugin, error) {
	f, err := os.Open(path)
	if os.IsNotExist(err) {
		return Plugin{}, err
	} else if err != nil {
		return Plugin{}, errors.Wrap(err, "failed to open index file")
	}
	defer f.Close()
	var plugin Plugin
	b, err := ioutil.ReadAll(f)
	if err != nil {
		return plugin, err
	}
	err = yaml.Unmarshal(b, &plugin)
	if err != nil {
		return plugin, errors.Wrap(err, "failed to decode plugin manifest")
	}
	return plugin, errors.Wrap(plugin.ValidatePlugin(plugin.Name), "plugin manifest validation error")
}
