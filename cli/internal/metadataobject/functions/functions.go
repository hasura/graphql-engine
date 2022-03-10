package functions

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
)

type FunctionConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *FunctionConfig {
	return &FunctionConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (f *FunctionConfig) Validate() error {
	return nil
}

func (f *FunctionConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(f.MetadataDir, f.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (f *FunctionConfig) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	data, err := ioutil.ReadFile(filepath.Join(f.MetadataDir, f.Filename()))
	if err != nil {
		return nil, f.error(err)
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, f.error(err)
	}
	return map[string]interface{}{f.Key(): obj}, nil
}

func (f *FunctionConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	return metadataobject.DefaultExport(f, metadata, f.error, metadataobject.DefaultObjectTypeSequence)
}

func (f *FunctionConfig) Key() string {
	return metadataobject.FunctionsKey
}

func (f *FunctionConfig) Filename() string {
	return "functions.yaml"
}

func (f *FunctionConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(f.BaseDirectory(), f.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, f.error(err)
	}
	return files, nil
}

func (f *FunctionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: f, WriteDiffOpts: opts})
	if err != nil {
		return f.error(err)
	}
	return nil
}

func (f *FunctionConfig) BaseDirectory() string {
	return f.MetadataDir
}

func (f *FunctionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(f, err, additionalContext...)
}
