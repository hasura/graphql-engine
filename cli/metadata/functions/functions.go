package functions

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "functions.yaml"
)

type FunctionConfig struct {
	MetadataDir string
}

func New(baseDir string) *FunctionConfig {
	return &FunctionConfig{
		MetadataDir: baseDir,
	}
}

func (a *FunctionConfig) Validate() error {
	return nil
}

func (a *FunctionConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.Functions)
}

func (a *FunctionConfig) Export(metadata dbTypes.Metadata) error {
	data, err := yaml.Marshal(metadata.Functions)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}
