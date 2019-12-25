package tables

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
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

func (a *TableConfig) Validate() error {
	return nil
}

func (a *TableConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.Tables)
}

func (a *TableConfig) Export(metadata dbTypes.Metadata) error {
	data, err := yaml.Marshal(metadata.Tables)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}
