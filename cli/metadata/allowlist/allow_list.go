package allowlist

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
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

func (a *AllowListConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	return gyaml.Unmarshal(data, &metadata.AllowList)
}

func (a *AllowListConfig) Export(metadata dbTypes.Metadata) error {
	data, err := yaml.Marshal(metadata.AllowList)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}
