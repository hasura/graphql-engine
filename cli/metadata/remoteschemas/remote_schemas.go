package remoteschemas

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
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

func (r *RemoteSchemaConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(r.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.RemoteSchemas)
}

func (r *RemoteSchemaConfig) Export(metadata dbTypes.Metadata) (types.MetadataFiles, error) {
	if metadata.RemoteSchemas == nil {
		metadata.RemoteSchemas = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(metadata.RemoteSchemas)
	if err != nil {
		return types.MetadataFiles{}, err
	}
	return types.MetadataFiles{
		{
			Path:    filepath.Join(r.MetadataDir, fileName),
			Content: data,
		},
	}, nil
}
