package remoteschemas

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "remote_schemas.yaml"
)

type RemoteSchemaConfig struct {
	MetadataDir string
}

func New(baseDir string) *RemoteSchemaConfig {
	return &RemoteSchemaConfig{
		MetadataDir: baseDir,
	}
}

func (a *RemoteSchemaConfig) Validate() error {
	return nil
}

func (a *RemoteSchemaConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.RemoteSchemas)
}

func (a *RemoteSchemaConfig) Export(metadata dbTypes.Metadata) error {
	data, err := yaml.Marshal(metadata.RemoteSchemas)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}
