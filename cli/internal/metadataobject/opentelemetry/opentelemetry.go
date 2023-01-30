package opentelemetry

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
)

type OpentelemetryConfig struct {
	MetadataDir string
	logger      *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *OpentelemetryConfig {
	return &OpentelemetryConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (o *OpentelemetryConfig) Validate() error {
	return nil
}

func (o *OpentelemetryConfig) CreateFiles() error {
	var op errors.Op = "opentelemetry.OpentelemetryConfig.CreateFiles"
	node := yaml.Node{Kind: yaml.MappingNode}
	data, err := yaml.Marshal(node)
	if err != nil {
		return errors.E(op, err)
	}
	if err := ioutil.WriteFile(filepath.Join(o.MetadataDir, o.Filename()), data, 0644); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (o *OpentelemetryConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "opentelemetry.OpentelemetryConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(o.MetadataDir, o.Filename()))
	if err != nil {
		return nil, errors.E(op, err)
	}
	var obj interface{}
	if err = yaml.Unmarshal(data, &obj); err != nil {
		return nil, errors.E(op, errors.KindBadInput, o.error(err))
	}
	return map[string]interface{}{o.Key(): obj}, nil
}

func (o *OpentelemetryConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "opentelemetry.OpentelemetryConfig.Export"
	m, err := metadataobject.DefaultExport(o, metadata, o.error, metadataobject.DefaultObjectTypeMapping)
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	return m, nil
}

func (o *OpentelemetryConfig) GetFiles() ([]string, error) {
	var op errors.Op = "opentelemetry.OpentelemetryConfig.GetFiles"
	rootFile := filepath.Join(o.BaseDirectory(), o.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	return files, nil
}

func (o *OpentelemetryConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "opentelemetry.OpentelemetryConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: o, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, o.error(err))
	}
	return nil
}

func (o *OpentelemetryConfig) Filename() string {
	return "opentelemetry.yaml"
}

func (o *OpentelemetryConfig) Key() string {
	return metadataobject.OpentelemetryKey
}

func (o *OpentelemetryConfig) BaseDirectory() string {
	return o.MetadataDir
}

func (o *OpentelemetryConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
