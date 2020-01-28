package version

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "version.yaml"
)

type Version struct {
	Version int `json:"version" yaml:"version"`
}

type VersionConfig struct {
	MetadataDir string
}

func New(baseDir string) *VersionConfig {
	return &VersionConfig{
		MetadataDir: baseDir,
	}
}

func (a *VersionConfig) Validate() error {
	return nil
}

func (a *VersionConfig) CreateFiles() error {
	v := Version{
		Version: 2,
	}
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

func (a *VersionConfig) Build(metadata *dbTypes.Metadata) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	var v Version
	err = gyaml.Unmarshal(data, &v)
	if err != nil {
		return err
	}
	metadata.Version = v.Version
	return nil
}

func (a *VersionConfig) Export(metadata yaml.MapSlice) (types.MetadataFiles, error) {
	var version int
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "version" {
			continue
		}
		version = item.Value.(int)
	}
	v := Version{
		Version: version,
	}
	data, err := yaml.Marshal(v)
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
