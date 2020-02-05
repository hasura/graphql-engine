package querycollections

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
	fileName string = "query_collections.yaml"
)

type QueryCollectionConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *QueryCollectionConfig {
	return &QueryCollectionConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
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

func (q *QueryCollectionConfig) Build(metadata *types.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(q.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.QueryCollections)
}

func (q *QueryCollectionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
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
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(q.MetadataDir, fileName): data,
	}, nil
}
