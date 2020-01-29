package tables

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "tables.yaml"
)

type TableConfig struct {
	MetadataDir string
}

func New(baseDir string) *TableConfig {
	return &TableConfig{
		MetadataDir: baseDir,
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

func (t *TableConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.Tables)
}

func (t *TableConfig) Export(metadata yaml.MapSlice) (types.MetadataFiles, error) {
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
		return types.MetadataFiles{}, err
	}
	return types.MetadataFiles{
		{
			Path:    filepath.Join(t.MetadataDir, fileName),
			Content: data,
		},
	}, nil
}
