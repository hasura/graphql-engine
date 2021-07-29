package querytags

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

type MetadataObject struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *MetadataObject {
	return &MetadataObject{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (o *MetadataObject) Validate() error {
	return nil
}

func (o *MetadataObject) CreateFiles() error {
	var v interface{}
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(o.MetadataDir, o.Filename()), data, 0644)
	return err
}

func (o *MetadataObject) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(o.MetadataDir, o.Filename()))
	if err != nil {
		return o.error(err)
	}
	item := yaml.MapItem{
		Key: o.Key(),
	}
	var obj yaml.MapSlice
	if err := yaml.Unmarshal(data, &obj); err != nil {
		return o.error(err)
	}
	if len(obj) > 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (o *MetadataObject) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var queryTags interface{}
	for _, item := range metadata {
		if k, ok := item.Key.(string); ok && k == o.Key() {
			queryTags = item.Value
		}
	}
	if queryTags == nil {
		o.logger.WithFields(logrus.Fields{
			"object": o.Key(),
			"reason": "not found in metadata",
		}).Debugf("skipped building %s", o.Key())
		return nil, nil
	}
	data, err := yaml.Marshal(queryTags)
	if err != nil {
		return nil, o.error(err)
	}
	return map[string][]byte{
		filepath.Join(o.MetadataDir, o.Filename()): data,
	}, nil
}

func (o *MetadataObject) Key() string {
	return "query_tags"
}

func (o *MetadataObject) Filename() string {
	return "query_tags.yaml"
}

func (o *MetadataObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
