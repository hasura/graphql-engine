package apilimits

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
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
	var op errors.Op = "apilimits.MetadataObject.CreateFiles"
	var v interface{}
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(o.MetadataDir, o.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

type apiLimitsObject struct {
	Disabled   yaml.Node `yaml:"disabled,omitempty"`
	RateLimit  yaml.Node `yaml:"rate_limit,omitempty"`
	DepthLimit yaml.Node `yaml:"depth_limit,omitempty"`
	NodeLimit  yaml.Node `yaml:"node_limit,omitempty"`
	TimeLimit  yaml.Node `yaml:"time_limit,omitempty"`
	BatchLimit yaml.Node `yaml:"batch_limit,omitempty"`
}

func (o *MetadataObject) Build() (map[string]interface{}, error) {
	var op errors.Op = "apilimits.MetadataObject.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(o.MetadataDir, o.Filename()))
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	// The reason for loosely typing this variable to a struct rather than using a catch-all yaml.Node
	// is because, if we were to do something like
	// data, _ := ioutil.ReadFile(filepath.Join(o.MetadataDir, o.Filename()))
	// var obj yaml.Node
	// err = yaml.Unmarshal(data, &obj)
	// then the yaml.Node.Kind will be a Document Node and therefore the final return value will be
	// map[string]interface{}{ o.Key: A Document Node }
	// if we call a yaml.Marshal on this return value it'll fail with the following error
	// `yaml: expected SCALAR, SEQUENCE-START, MAPPING-START, or ALIAS, but got document start`
	// which is fair, because a map will be parsed to a Mapping Node and yaml should not have a "Document Node" as
	// child of a Mapping Node and hence the error
	// This pattern is repeated for objects where it's child is a MappingNode
	var obj apiLimitsObject
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, o.error(err))
	}
	return map[string]interface{}{o.Key(): obj}, nil
}

func (o *MetadataObject) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "apilimits.MetadataObject.Export"
	b, err := metadataobject.DefaultExport(o, metadata, o.error, metadataobject.DefaultObjectTypeMapping)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return b, nil
}

func (o *MetadataObject) Key() string {
	return metadataobject.APILimitsKey
}

func (o *MetadataObject) Filename() string {
	return "api_limits.yaml"
}

func (o *MetadataObject) GetFiles() ([]string, error) {
	var op errors.Op = "apilimits.MetadataObject.GetFiles"
	rootFile := filepath.Join(o.BaseDirectory(), o.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	return files, nil
}

func (o *MetadataObject) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "apilimits.MetadataObject.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: o, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, o.error(err))
	}
	return nil
}

func (o *MetadataObject) BaseDirectory() string {
	return o.MetadataDir
}

func (o *MetadataObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
