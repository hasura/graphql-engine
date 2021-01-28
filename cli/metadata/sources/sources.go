package sources

import (
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/metadata/tables"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "sources.yaml"
)

type SourceConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *SourceConfig {
	return &SourceConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
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
	*metadata = append(*metadata, item)
	return nil
}

func (t *SourceConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var sources interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "sources" {
			continue
		}
		sources = item.Value
	}
	if sources == nil {
		sources = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(sources)
	if err != nil {
		return nil, err
	}

	// clear old tables.yaml and functions.yaml files if exists
	if f, _ := os.Stat(filepath.Join(t.MetadataDir, tables.MetadataFilename)); f != nil {
		os.Remove(filepath.Join(t.MetadataDir, tables.MetadataFilename))
	}
	if f, _ := os.Stat(filepath.Join(t.MetadataDir, tables.MetadataFilename)); f != nil {
		os.Remove(filepath.Join(t.MetadataDir, tables.MetadataFilename))
	}

	return map[string][]byte{
		filepath.Join(t.MetadataDir, fileName): data,
	}, nil
}

func (t *SourceConfig) Name() string {
	return "sources"
}
