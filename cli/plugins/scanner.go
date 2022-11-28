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

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/plugins/paths"
	"github.com/spf13/afero"
)

func (c *Config) findPluginManifestFiles(indexDir string) ([]string, error) {
	var op errors.Op = "plugins.Config.findPluginManifestFiles"
	c.Logger.Debugf("finding plugin manifest files in directory %v", indexDir)
	var out []string
	fs := afero.Afero{
		Fs: afero.NewOsFs(),
	}
	err := fs.Walk(indexDir, func(path string, info os.FileInfo, err error) error {
		if info == nil {
			if err != nil {
				return errors.E(op, err)
			}
			return nil
		}
		if info.Mode().IsRegular() && filepath.Ext(info.Name()) == paths.ManifestExtension {
			out = append(out, path)
		}
		return nil
	})
	if err != nil {
		return nil, errors.E(op, err)
	}

	return out, nil
}

// LoadPluginListFromFS will parse and retrieve all plugin files.
func (c *Config) LoadPluginListFromFS(indexDir string) (Plugins, error) {
	var op errors.Op = "plugins.Config.LoadPluginListFromFS"
	indexDir, err := filepath.EvalSymlinks(indexDir)
	if err != nil {
		return nil, errors.E(op, err)
	}

	files, err := c.findPluginManifestFiles(indexDir)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("failed to scan plugins in index directory: %w", err))
	}

	return c.LoadPlugins(files), nil
}

// LoadPluginByName loads a plugins index file by its name. When plugin
// file not found, it returns an error that can be checked with stderrors.Is(err, fs.ErrNotExist)
func (c *Config) LoadPluginByName(pluginName string) (*PluginVersions, error) {
	var op errors.Op = "plugins.Config.LoadPluginByName"
	c.Logger.Debugf("loading plugin %s", pluginName)
	if !IsSafePluginName(pluginName) {
		return nil, errors.E(op, fmt.Errorf("plugin name %q not allowed", pluginName))
	}

	files, err := c.findPluginManifestFiles(c.Paths.IndexPluginsPath())
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("failed to scan plugins in index directory: %w", err))
	}

	ps := c.LoadPlugins(files, pluginName)
	if _, ok := ps[pluginName]; !ok {
		return nil, errors.E(op, os.ErrNotExist)
	}

	return ps[pluginName], nil
}

func (c *Config) LoadPlugins(files []string, pluginName ...string) Plugins {
	c.Logger.Debugf("loading plugins")
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
// returns an error that can be checked with stderrors.Is(err, fs.ErrNotExist).
func (c *Config) ReadPluginFromFile(path string) (Plugin, error) {
	var op errors.Op = "plugins.Config.ReadPluginFromFile"
	f, err := os.Open(path)
	if stderrors.Is(err, fs.ErrNotExist) {
		return Plugin{}, errors.E(op, err)
	} else if err != nil {
		return Plugin{}, errors.E(op, fmt.Errorf("failed to open index file: %w", err))
	}
	defer f.Close()
	var plugin Plugin
	b, err := ioutil.ReadAll(f)
	if err != nil {
		return plugin, errors.E(op, err)
	}
	err = yaml.Unmarshal(b, &plugin)
	if err != nil {
		return plugin, errors.E(op, fmt.Errorf("failed to decode plugin manifest: %w", err))
	}
	plugin.ParseVersion()
	err = plugin.ValidatePlugin(plugin.Name)
	if err != nil {
		return plugin, errors.E(op, fmt.Errorf("plugin manifest validation error: %w", err))
	}
	return plugin, nil
}
