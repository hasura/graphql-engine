package restendpoints

import (
	"io/ioutil"
	"path/filepath"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli"
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

func (t *RestEndpointsConfig) Validate() error {
	return nil
}

func (t *RestEndpointsConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(t.MetadataDir, MetadataFilename), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *RestEndpointsConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, MetadataFilename))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key:   "rest_endpoints",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return err
	}
	*metadata = append(*metadata, item)
	return nil
}

func (t *RestEndpointsConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
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
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(t.MetadataDir, MetadataFilename): data,
	}, nil
}

func (t *RestEndpointsConfig) Name() string {
	return "rest_endpoints"
}
