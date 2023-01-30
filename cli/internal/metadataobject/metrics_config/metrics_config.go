package metricsconfig

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
)

type MetricsConfigObject struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *MetricsConfigObject {
	return &MetricsConfigObject{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (m *MetricsConfigObject) Validate() error {
	return nil
}

func (m *MetricsConfigObject) CreateFiles() error {
	var op errors.Op = "metricsconfig.MetricsConfigObject.CreateFiles"
	var v interface{}
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(m.MetadataDir, m.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *MetricsConfigObject) Build() (map[string]interface{}, error) {
	var op errors.Op = "metricsconfig.MetricsConfigObject.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(m.MetadataDir, m.Filename()))
	if err != nil {
		return nil, errors.E(op, m.error(err))
	}
	var obj map[string]yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, m.error(err))
	}
	// if we have a metrics_config.yaml file which is empty
	// do not set this key in the generated metadata
	// if we do that and it will effectively send the following JSON to server
	// {
	//   "version": 3,
	//   "sources": [],
	//   "metrics_config": {}
	// }
	// This will result in a server error like the following
	// {
	//   "code": "parse-failed",
	//   "error": "the key 'analyze_query_variables' was not present",
	//   "path": "$.args.metadata.metrics_config"
	// }
	// The is a deviation from the normal server behavior
	if len(obj) == 0 {
		return nil, nil
	}
	return map[string]interface{}{m.Key(): obj}, nil
}

func (m *MetricsConfigObject) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "metricsconfig.MetricsConfigObject.Export"
	b, err := metadataobject.DefaultExport(m, metadata, m.error, metadataobject.DefaultObjectTypeMapping)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return b, nil
}

func (m *MetricsConfigObject) Key() string {
	return metadataobject.MetricsConfigKey
}

func (m *MetricsConfigObject) Filename() string {
	return "metrics_config.yaml"
}

func (m *MetricsConfigObject) GetFiles() ([]string, error) {
	var op errors.Op = "metricsconfig.MetricsConfigObject.GetFiles"
	rootFile := filepath.Join(m.BaseDirectory(), m.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, m.error(err))
	}
	return files, nil
}

func (m *MetricsConfigObject) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "metricsconfig.MetricsConfigObject.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: m, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, m.error(err))
	}
	return nil
}

func (m *MetricsConfigObject) BaseDirectory() string {
	return m.MetadataDir
}

func (m *MetricsConfigObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(m, err, additionalContext...)
}
