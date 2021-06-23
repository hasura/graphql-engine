package querycollections

import (
	"io/ioutil"
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
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

func (q *QueryCollectionConfig) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(q.MetadataDir, fileName))
	if err != nil {
		return q.Error(err)
	}
	item := yaml.MapItem{
		Key: "query_collections",
	}
	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return q.Error(err)
	}
	if len(obj) != 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (q *QueryCollectionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
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
		return nil, q.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(q.MetadataDir, fileName)): data,
	}, nil
}

func (q *QueryCollectionConfig) Name() string {
	return "query_collections"
}

func (q *QueryCollectionConfig) Error(err error, additionalContext ...string) errors2.ErrParsingMetadataObject {
	return errors2.NewErrParsingMetadataObject(q.Name(), fileName, additionalContext, err)
}
