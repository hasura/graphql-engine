package tables

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

type TableConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *TableConfig {
	return &TableConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (t *TableConfig) Validate() error {
	return nil
}

func (t *TableConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(t.MetadataDir, t.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *TableConfig) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, t.Filename()))
	if err != nil {
		return t.error(err)
	}
	item := yaml.MapItem{
		Key:   "tables",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return t.error(err)
	}
	*metadata = append(*metadata, item)
	return nil
}

func (t *TableConfig) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var tables interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "tables" {
			continue
		}
		tables = item.Value
	}
	if tables == nil {
		tables = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(tables)
	if err != nil {
		return nil, t.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(t.MetadataDir, t.Filename())): data,
	}, nil
}

func (t *TableConfig) Filename() string {
	return "tables.yaml"
}

func (t *TableConfig) Key() string {
	return "tables"
}

func (t *TableConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(t.BaseDirectory(), t.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, t.error(err)
	}
	return files, nil
}

func (t *TableConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: t, WriteDiffOpts: opts})
	if err != nil {
		return t.error(err)
	}
	return nil
}

func (t *TableConfig) BaseDirectory() string {
	return t.MetadataDir
}

func (t *TableConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(t, err, additionalContext...)
}
