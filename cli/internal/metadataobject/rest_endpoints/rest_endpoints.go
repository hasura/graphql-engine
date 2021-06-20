package restendpoints

import (
	"io/ioutil"
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

const (
	MetadataFilename string = "rest_endpoints.yaml"
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
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(re.MetadataDir, MetadataFilename), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (re *RestEndpointsConfig) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(re.MetadataDir, MetadataFilename))
	if err != nil {
		return re.Error(err)
	}
	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return re.Error(err)
	}
	if len(obj) > 0 {
		item := yaml.MapItem{
			Key:   "rest_endpoints",
			Value: []yaml.MapSlice{},
		}
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (re *RestEndpointsConfig) Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
	var restEndpoints interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "rest_endpoints" {
			continue
		}
		restEndpoints = item.Value
	}
	if restEndpoints == nil {
		restEndpoints = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(restEndpoints)
	if err != nil {
		return nil, re.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(re.MetadataDir, MetadataFilename)): data,
	}, nil
}

func (re *RestEndpointsConfig) Name() string {
	return "rest_endpoints"
}

func (re *RestEndpointsConfig) Error(err error, additionalContext ...string) errors2.ErrParsingMetadataObject {
	return errors2.NewErrParsingMetadataObject(re.Name(), MetadataFilename, additionalContext, err)
}
