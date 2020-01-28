package functions

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
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

func (f *FunctionConfig) Validate() error {
	return nil
}

func (f *FunctionConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(f.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (f *FunctionConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(f.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.Functions)
}

func (f *FunctionConfig) Export(metadata yaml.MapSlice) (types.MetadataFiles, error) {
	var functions interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "functions" {
			continue
		}
		functions = item.Value
	}
	if functions == nil {
		functions = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(functions)
	if err != nil {
		return types.MetadataFiles{}, err
	}
	return types.MetadataFiles{
		{
			Path:    filepath.Join(f.MetadataDir, fileName),
			Content: data,
		},
	}, nil
}
