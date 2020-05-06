package remoteschemas

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "remote_schemas.yaml"
)

type RemoteSchemaConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *RemoteSchemaConfig {
	return &RemoteSchemaConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (r *RemoteSchemaConfig) Validate() error {
	return nil
}

func (r *RemoteSchemaConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(r.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (r *RemoteSchemaConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(r.MetadataDir, fileName))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key: "remote_schemas",
	}
	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return err
	}
	if len(obj) != 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (r *RemoteSchemaConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var remoteSchemas interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "remote_schemas" {
			continue
		}
		remoteSchemas = item.Value
	}
	if remoteSchemas == nil {
		remoteSchemas = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(remoteSchemas)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(r.MetadataDir, fileName): data,
	}, nil
}

func (r *RemoteSchemaConfig) Name() string {
	return "remote_schemas"
}
