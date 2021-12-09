package graphqlschemaintrospection

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
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
	if err != nil {
		return err
	}
	return nil
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
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return o.error(err)
	}
	if len(obj) > 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (o *MetadataObject) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var graphqlSchemaIntroSpection interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != o.Key() {
			continue
		}
		graphqlSchemaIntroSpection = item.Value
	}
	if graphqlSchemaIntroSpection == nil {
		o.logger.WithFields(logrus.Fields{
			"object": o.Key(),
			"reason": "not found in metadata",
		}).Debugf("skipped building %s", o.Key())
		return nil, nil
	}
	data, err := yaml.Marshal(graphqlSchemaIntroSpection)
	if err != nil {
		return nil, o.error(err)
	}
	return map[string][]byte{
		filepath.Join(o.MetadataDir, o.Filename()): data,
	}, nil
}

func (o *MetadataObject) Key() string {
	return "graphql_schema_introspection"
}

func (o *MetadataObject) Filename() string {
	return "graphql_schema_introspection.yaml"
}

func (o *MetadataObject) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(o.BaseDirectory(), o.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, o.error(err)
	}
	return files, nil
}

func (o *MetadataObject) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: o, WriteDiffOpts: opts})
	if err != nil {
		return o.error(err)
	}
	return nil
}

func (o *MetadataObject) BaseDirectory() string {
	return o.MetadataDir
}

func (o *MetadataObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
