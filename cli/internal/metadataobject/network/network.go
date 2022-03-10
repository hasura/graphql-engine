package network

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
)

type NetworkObject struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *NetworkObject {
	return &NetworkObject{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (o *NetworkObject) Validate() error {
	return nil
}

func (o *NetworkObject) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(o.MetadataDir, o.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

type networkObject struct {
	TLSAllowlist yaml.Node `yaml:"tls_allowlist,omitempty"`
}

func (o *NetworkObject) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	data, err := ioutil.ReadFile(filepath.Join(o.MetadataDir, o.Filename()))
	if err != nil {
		return nil, o.error(err)
	}
	var obj networkObject
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, o.error(err)
	}
	return map[string]interface{}{o.Key(): obj}, nil
}

func (o *NetworkObject) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	return metadataobject.DefaultExport(o, metadata, o.error, metadataobject.DefaultObjectTypeMapping)
}

func (o *NetworkObject) Key() string {
	return metadataobject.NetworkKey
}

func (o *NetworkObject) Filename() string {
	return "network.yaml"
}

func (o *NetworkObject) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(o.BaseDirectory(), o.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, o.error(err)
	}
	return files, nil
}

func (o *NetworkObject) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: o, WriteDiffOpts: opts})
	if err != nil {
		return o.error(err)
	}
	return nil
}

func (o *NetworkObject) BaseDirectory() string {
	return o.MetadataDir
}

func (o *NetworkObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
