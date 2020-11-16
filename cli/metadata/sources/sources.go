package sources

import (
	"io/ioutil"
	"path/filepath"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "sources.yaml"
)

type SourceConfig struct {
	MetadataDir string

	HasDatasources bool

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *SourceConfig {
	ec.Version.GetServerFeatureFlags()
	return &SourceConfig{
		MetadataDir:    baseDir,
		HasDatasources: ec.Version.ServerFeatureFlags.HasDatasources,
		logger:         ec.Logger,
	}
}

func (t *SourceConfig) Validate() error {
	return nil
}

func (t *SourceConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(t.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *SourceConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, fileName))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key:   "sources",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return err
	}
	if t.HasDatasources {
		*metadata = append(*metadata, item)
	} else {
		tables := yaml.MapItem{
			Key:   "tables",
			Value: []yaml.MapSlice{},
		}
		functions := yaml.MapItem{
			Key:   "functions",
			Value: []yaml.MapSlice{},
		}
		for _, source := range item.Value.([]interface{}) { // remove for loop it for loop not needed
			source := source.(map[interface{}]interface{})
			if source["name"] == "default" { // remove if condition it for loop not needed
				if source["functions"] != nil {
					functions.Value = source["functions"]
				}
				if source["tables"] != nil {
					tables.Value = source["tables"]
				}
				break
			}
		}
		*metadata = append(*metadata, tables)
		*metadata = append(*metadata, functions)
		// split item.Value into tables and functions
		// and append tables and functions into metadata
	}
	return nil
}

func (t *SourceConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var sources interface{}

	if !t.HasDatasources {
		name := yaml.MapItem{
			Key:   "name",
			Value: "default",
		}
		tables := yaml.MapItem{
			Key:   "tables",
			Value: make([]interface{}, 0),
		}
		functions := yaml.MapItem{
			Key:   "functions",
			Value: make([]interface{}, 0),
		}
		for _, item := range metadata {
			key, _ := item.Key.(string)
			if key == "tables" {
				tables = item
			} else if key == "functions" {
				functions = item
			}
		}

		sources = []yaml.MapSlice{yaml.MapSlice{name, tables, functions}}
		// add tables and functions into sources
		// add name : default to sources

	} else {
		for _, item := range metadata {
			k, ok := item.Key.(string)
			if !ok || k != "sources" {
				continue
			}
			sources = item.Value
			break
		}
	}

	if sources == nil {
		sources = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(sources)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(t.MetadataDir, fileName): data,
	}, nil
}

func (t *SourceConfig) Name() string {
	return "sources"
}
