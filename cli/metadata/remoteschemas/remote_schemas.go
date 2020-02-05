package remoteschemas

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/types"
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

func (r *RemoteSchemaConfig) Build(metadata *types.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(r.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.RemoteSchemas)
}

func (r *RemoteSchemaConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var remoteSchemas interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "allowlist" {
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
