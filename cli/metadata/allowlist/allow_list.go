package allowlist

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "allow_list.yaml"
)

type AllowListConfig struct {
	MetadataDir string
}

func New(baseDir string) *AllowListConfig {
	return &AllowListConfig{
		MetadataDir: baseDir,
	}
}

func (a *AllowListConfig) Validate() error {
	return nil
}

func (a *AllowListConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *AllowListConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.AllowList)
}

func (a *AllowListConfig) Export(metadata yaml.MapSlice) (types.MetadataFiles, error) {
	var allowList interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "remote_schemas" {
			continue
		}
		allowList = item.Value
	}
	if allowList == nil {
		allowList = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(allowList)
	if err != nil {
		return types.MetadataFiles{}, err
	}
	return types.MetadataFiles{
		{
			Path:    filepath.Join(a.MetadataDir, fileName),
			Content: data,
		},
	}, nil
}
