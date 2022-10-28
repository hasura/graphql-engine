package tables

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
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

func (t *TableConfig) Build() (map[string]interface{}, error) {
	data, err := metadataobject.ReadMetadataFile(filepath.Join(t.MetadataDir, t.Filename()))
	if err != nil {
		return nil, t.error(err)
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, t.error(err)
	}
	return map[string]interface{}{t.Key(): obj}, nil
}

func (t *TableConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	return metadataobject.DefaultExport(t, metadata, t.error, metadataobject.DefaultObjectTypeSequence)
}

func (t *TableConfig) Filename() string {
	return "tables.yaml"
}

func (t *TableConfig) Key() string {
	return metadataobject.TablesKey
}

func (t *TableConfig) GetFiles() ([]string, error) {
	rootFile := filepath.Join(t.BaseDirectory(), t.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, t.error(err)
	}
	return files, nil
}

func (t *TableConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
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
