package apilimits

import (
	"io/ioutil"
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

const (
	MetadataFilename string = "api_limits.yaml"
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
	var v interface{}
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(o.MetadataDir, MetadataFilename), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (o *MetadataObject) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(o.MetadataDir, MetadataFilename))
	if err != nil {
		return o.Error(err)
	}
	item := yaml.MapItem{
		Key: o.Name(),
	}
	var obj yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return o.Error(err)
	}
	if len(obj) > 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (o *MetadataObject) Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
	var apiLimits interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != o.Name() {
			continue
		}
		apiLimits = item.Value
	}
	if apiLimits == nil {
		o.logger.WithFields(logrus.Fields{
			"object": o.Name(),
			"reason": "not found in metadata",
		}).Debugf("skipped building %s", o.Name())
		return nil, nil
	}
	data, err := yaml.Marshal(apiLimits)
	if err != nil {
		return nil, o.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(o.MetadataDir, MetadataFilename)): data,
	}, nil
}

func (o *MetadataObject) Name() string {
	return "api_limits"
}

func (o *MetadataObject) Error(err error, additionalContext ...string) errors2.ErrParsingMetadataObject {
	return errors2.NewErrParsingMetadataObject(o.Name(), MetadataFilename, additionalContext, err)
}
