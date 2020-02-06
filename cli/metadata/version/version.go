package version

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
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

func New(ec *cli.ExecutionContext, baseDir string) *VersionConfig {
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

func (a *VersionConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	var v Version
	err = yaml.Unmarshal(data, &v)
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key:   "version",
		Value: v.Version,
	}
	*metadata = append(*metadata, item)
	return nil
}

func (a *VersionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
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
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(a.MetadataDir, fileName): data,
	}, nil
}

func (a *VersionConfig) Name() string {
	return "version"
}
