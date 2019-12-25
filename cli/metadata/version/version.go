package version

import (
	"io/ioutil"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "version.yaml"
)

type Version struct {
	Version string `json:"version" yaml:"version"`
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

func (a *VersionConfig) Export(metadata dbTypes.Metadata) error {
	v := Version{
		Version: metadata.Version,
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
