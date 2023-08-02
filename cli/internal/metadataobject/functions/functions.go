package functions

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
	var op errors.Op = "functions.FunctionConfig.CreateFiles"
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(f.MetadataDir, f.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (f *FunctionConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "functions.FunctionConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(f.MetadataDir, f.Filename()))
	if err != nil {
		return nil, errors.E(op, f.error(err))
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, f.error(err))
	}
	return map[string]interface{}{f.Key(): obj}, nil
}

func (f *FunctionConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "functions.FunctionConfig.Export"
	v, err := metadataobject.DefaultExport(f, metadata, f.error, metadataobject.DefaultObjectTypeSequence)
	if err != nil {
		return nil, errors.E(op, f.error(err))
	}
	return v, nil
}

func (f *FunctionConfig) Key() string {
	return metadataobject.FunctionsKey
}

func (f *FunctionConfig) Filename() string {
	return "functions.yaml"
}

func (f *FunctionConfig) GetFiles() ([]string, error) {
	var op errors.Op = "functions.FunctionConfig.GetFiles"
	rootFile := filepath.Join(f.BaseDirectory(), f.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, f.error(err))
	}
	return files, nil
}

func (f *FunctionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "functions.FunctionConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: f, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, f.error(err))
	}
	return nil
}

func (f *FunctionConfig) BaseDirectory() string {
	return f.MetadataDir
}

func (f *FunctionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(f, err, additionalContext...)
}
