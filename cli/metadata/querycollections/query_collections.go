package querycollections

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "query_collections.yaml"
)

type QueryCollectionConfig struct {
	MetadataDir string
}

func New(baseDir string) *QueryCollectionConfig {
	return &QueryCollectionConfig{
		MetadataDir: baseDir,
	}
}

func (a *QueryCollectionConfig) Validate() error {
	return nil
}

func (a *QueryCollectionConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.QueryCollections)
}

func (a *QueryCollectionConfig) Export(metadata dbTypes.Metadata) error {
	data, err := yaml.Marshal(metadata.QueryCollections)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}
