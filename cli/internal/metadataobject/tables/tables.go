package tables

import (
	"io/ioutil"
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

const (
	MetadataFilename string = "tables.yaml"
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
	err = ioutil.WriteFile(filepath.Join(t.MetadataDir, MetadataFilename), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *TableConfig) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, MetadataFilename))
	if err != nil {
		return t.Error(err)
	}
	item := yaml.MapItem{
		Key:   "tables",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return t.Error(err)
	}
	*metadata = append(*metadata, item)
	return nil
}

func (t *TableConfig) Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
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
		return nil, t.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(t.MetadataDir, MetadataFilename)): data,
	}, nil
}

func (t *TableConfig) Name() string {
	return "tables"
}

func (t *TableConfig) Error(err error, additionalContext ...string) errors2.ErrParsingMetadataObject {
	return errors2.NewErrParsingMetadataObject(t.Name(), MetadataFilename, additionalContext, err)
}
