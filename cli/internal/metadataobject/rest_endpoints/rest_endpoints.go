package restendpoints

import (
	"bytes"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
)

type RestEndpointsConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *RestEndpointsConfig {
	return &RestEndpointsConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (re *RestEndpointsConfig) Validate() error {
	return nil
}

func (re *RestEndpointsConfig) CreateFiles() error {
	var op errors.Op = "restendpoints.RestEndpointsConfig.CreateFiles"
	v := make([]interface{}, 0)
	buf := new(bytes.Buffer)
	err := metadataobject.GetEncoder(buf).Encode(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(re.MetadataDir, re.Filename()), buf.Bytes(), 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (re *RestEndpointsConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "restendpoints.RestEndpointsConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(re.MetadataDir, re.Filename()))
	if err != nil {
		return nil, errors.E(op, re.error(err))
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, re.error(err))
	}
	if len(obj) == 0 {
		return nil, nil
	}
	return map[string]interface{}{re.Key(): obj}, nil
}

func (re *RestEndpointsConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "restendpoints.RestEndpointsConfig.Export"
	b, err := metadataobject.DefaultExport(re, metadata, re.error, metadataobject.DefaultObjectTypeSequence)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return b, nil
}

func (re *RestEndpointsConfig) Key() string {
	return metadataobject.RestEndpointsKey
}

func (re *RestEndpointsConfig) Filename() string {
	return "rest_endpoints.yaml"
}

func (re *RestEndpointsConfig) GetFiles() ([]string, error) {
	var op errors.Op = "restendpoints.RestEndpointsConfig.GetFiles"
	rootFile := filepath.Join(re.BaseDirectory(), re.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, re.error(err))
	}
	return files, nil
}

func (re *RestEndpointsConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "restendpoints.RestEndpointsConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: re, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, re.error(err))
	}
	return nil
}

func (re *RestEndpointsConfig) BaseDirectory() string {
	return re.MetadataDir
}

func (re *RestEndpointsConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(re, err, additionalContext...)
}
