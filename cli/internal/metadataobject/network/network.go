package network

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
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

func (o *NetworkObject) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(o.MetadataDir, o.Filename()))
	if err != nil {
		return o.error(err)
	}
	item := yaml.MapItem{
		Key: o.Key(),
	}
	var obj yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return o.error(err)
	}
	if len(obj) > 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (o *NetworkObject) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var network interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != o.Key() {
			continue
		}
		network = item.Value
	}
	if network == nil {
		o.logger.WithFields(logrus.Fields{
			"object": o.Key(),
			"reason": "not found in metadata",
		}).Debugf("skipped building %s", o.Key())
		return nil, nil
	}
	data, err := yaml.Marshal(network)
	if err != nil {
		return nil, o.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(o.MetadataDir, o.Filename())): data,
	}, nil
}

func (o *NetworkObject) Key() string {
	return "network"
}

func (o *NetworkObject) Filename() string {
	return "network.yaml"
}
func (o *NetworkObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
