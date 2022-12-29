package backendconfigs

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
)

type BackendConfigConfig struct {
	MetadataDir string
	logger      *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *BackendConfigConfig {
	return &BackendConfigConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (b *BackendConfigConfig) Validate() error {
	return nil
}

func (b *BackendConfigConfig) CreateFiles() error {
	var op errors.Op = "backendconfigs.BackendConfigConfig.CreateFiles"
	v := yaml.Node{Kind: yaml.MappingNode}
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(b.MetadataDir, b.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (b *BackendConfigConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "backendconfigs.BackendConfigConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(b.MetadataDir, b.Filename()))
	if err != nil {
		return nil, errors.E(op, b.error(err))
	}
	var obj interface{}
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, b.error(err))
	}
	return map[string]interface{}{b.Key(): obj}, nil
}

func (b *BackendConfigConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "backendconfigs.BackendConfigConfig.Export"
	m, err := metadataobject.DefaultExport(b, metadata, b.error, metadataobject.DefaultObjectTypeMapping)
	if err != nil {
		return nil, errors.E(op, b.error(err))
	}
	return m, nil
}

func (b *BackendConfigConfig) Key() string {
	return metadataobject.BackendConfigsKey
}

func (b *BackendConfigConfig) Filename() string {
	return "backend_configs.yaml"
}

func (b *BackendConfigConfig) GetFiles() ([]string, error) {
	var op errors.Op = "backendconfigs.BackendConfigConfig.GetFiles"
	rootFile := filepath.Join(b.BaseDirectory(), b.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, b.error(err))
	}
	return files, nil
}

func (b *BackendConfigConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "backendconfigs.BackendConfigConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: b, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, b.error(err))
	}
	return nil
}

func (b *BackendConfigConfig) BaseDirectory() string {
	return b.MetadataDir
}

func (b *BackendConfigConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(b, err, additionalContext...)
}
