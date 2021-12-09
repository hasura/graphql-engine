package querycollections

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
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
	err = ioutil.WriteFile(filepath.Join(q.MetadataDir, q.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (q *QueryCollectionConfig) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(q.MetadataDir, q.Filename()))
	if err != nil {
		return q.error(err)
	}
	item := yaml.MapItem{
		Key: "query_collections",
	}
	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return q.error(err)
	}
	if len(obj) != 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (q *QueryCollectionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
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
		return nil, q.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(q.MetadataDir, q.Filename())): data,
	}, nil
}

func (q *QueryCollectionConfig) Key() string {
	return "query_collections"
}

func (q *QueryCollectionConfig) Filename() string {
	return "query_collections.yaml"
}

func (q *QueryCollectionConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(q.BaseDirectory(), q.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, q.error(err)
	}
	return files, nil
}

func (q *QueryCollectionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: q, WriteDiffOpts: opts})
	if err != nil {
		return q.error(err)
	}
	return nil
}

func (q *QueryCollectionConfig) BaseDirectory() string {
	return q.MetadataDir
}

func (q *QueryCollectionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(q, err, additionalContext...)
}
