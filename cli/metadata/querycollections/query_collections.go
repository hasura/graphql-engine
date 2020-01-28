package querycollections

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
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

func (q *QueryCollectionConfig) Validate() error {
	return nil
}

func (q *QueryCollectionConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(q.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (q *QueryCollectionConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(q.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.QueryCollections)
}

func (q *QueryCollectionConfig) Export(metadata yaml.MapSlice) (types.MetadataFiles, error) {
	var queryCollections interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "query_collections" {
			continue
		}
		queryCollections = item.Value
	}
	if queryCollections == nil {
		queryCollections = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(queryCollections)
	if err != nil {
		return types.MetadataFiles{}, err
	}
	return types.MetadataFiles{
		{
			Path:    filepath.Join(q.MetadataDir, fileName),
			Content: data,
		},
	}, nil
}
