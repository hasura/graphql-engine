package tables

import (
	"io/ioutil"
	"path/filepath"

	"github.com/sirupsen/logrus"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "tables.yaml"
)

type TableConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *TableConfig {
	return &TableConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (t *TableConfig) Validate() error {
	return nil
}

func (t *TableConfig) CreateFiles() error {
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

func (t *TableConfig) Build(metadata *types.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.Tables)
}

func (t *TableConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var tables interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "tables" {
			continue
		}
		tables = item.Value
	}
	if tables == nil {
		tables = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(tables)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(t.MetadataDir, fileName): data,
	}, nil
}
