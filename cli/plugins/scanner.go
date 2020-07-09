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

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/plugins/paths"
	"github.com/pkg/errors"
	"github.com/spf13/afero"
)

func (c *Config) findPluginManifestFiles(indexDir string) ([]string, error) {
	var out []string
	fs := afero.Afero{
		Fs: afero.NewOsFs(),
	}
	fs.Walk(indexDir, func(path string, info os.FileInfo, err error) error {
		if info == nil {
			if err != nil {
				return err
			}
			return nil
		}
		if info.Mode().IsRegular() && filepath.Ext(info.Name()) == paths.ManifestExtension {
			out = append(out, path)
		}
		return nil
	})
	return out, nil
}

// LoadPluginListFromFS will parse and retrieve all plugin files.
func (c *Config) LoadPluginListFromFS(indexDir string) (Plugins, error) {
	indexDir, err := filepath.EvalSymlinks(indexDir)
	if err != nil {
		return nil, err
	}

	files, err := c.findPluginManifestFiles(indexDir)
	if err != nil {
		return nil, errors.Wrap(err, "failed to scan plugins in index directory")
	}

	return c.LoadPlugins(files), nil
}

// LoadPluginByName loads a plugins index file by its name. When plugin
// file not found, it returns an error that can be checked with os.IsNotExist.
func (c *Config) LoadPluginByName(pluginName string) (*PluginVersions, error) {
	if !IsSafePluginName(pluginName) {
		return nil, errors.Errorf("plugin name %q not allowed", pluginName)
	}

	files, err := c.findPluginManifestFiles(c.Paths.IndexPluginsPath())
	if err != nil {
		return nil, errors.Wrap(err, "failed to scan plugins in index directory")
	}

	ps := c.LoadPlugins(files, pluginName)
	if _, ok := ps[pluginName]; !ok {
		return nil, os.ErrNotExist
	}

	return ps[pluginName], nil
}

func (c *Config) LoadPlugins(files []string, pluginName ...string) Plugins {
	ps := Plugins{}
	for _, file := range files {
		p, err := c.ReadPluginFromFile(file)
		if err != nil {
			c.Logger.Debugf("failed to read or parse plugin manifest: %v", err)
			continue
		}
		if p.ParsedVersion == nil {
			c.Logger.Debugf("version of plugin %s cannot be nil", p.Name)
			continue
		}
		if len(pluginName) != 0 {
			var isFound bool
			for _, name := range pluginName {
				if name == p.Name {
					isFound = true
					break
				}
			}
			if !isFound {
				continue
			}
		}
		if _, ok := ps[p.Name]; !ok {
			ps[p.Name] = NewPluginVersions()
		}
		err = ps[p.Name].Append(p)
		if err != nil {
			c.Logger.Debugf("failed to append version %s for plugin %s: %v", p.Version, p.Name, err)
			continue
		}
	}
	return ps
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
	plugin.ParseVersion()
	return plugin, errors.Wrap(plugin.ValidatePlugin(plugin.Name), "plugin manifest validation error")
}
