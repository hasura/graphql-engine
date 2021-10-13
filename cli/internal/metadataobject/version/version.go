package version

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

type Version struct {
	Version int `json:"version" yaml:"version"`
}

type VersionConfig struct {
	MetadataDir string
}

func New(ec *cli.ExecutionContext, baseDir string) *VersionConfig {
	err := ec.Version.GetServerFeatureFlags()
	if err != nil {
		ec.Logger.Errorf("got error while creating instance of VersionConfig: %v", err)
		return nil
	}
	return &VersionConfig{
		MetadataDir: baseDir,
	}
}

func (a *VersionConfig) Validate() error {
	return nil
}

func (a *VersionConfig) CreateFiles() error {
	v := Version{
		Version: 3,
	}
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, a.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *VersionConfig) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, a.Filename()))
	if err != nil {
		return a.error(err)
	}
	var v Version
	err = yaml.Unmarshal(data, &v)
	if err != nil {
		return a.error(err)
	}
	item := yaml.MapItem{
		Key:   "version",
		Value: v.Version,
	}
	*metadata = append(*metadata, item)
	return nil
}

func (a *VersionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
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
		return nil, a.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(a.MetadataDir, a.Filename())): data,
	}, nil
}

func (a *VersionConfig) Key() string {
	return "version"
}

func (a *VersionConfig) Filename() string {
	return "version.yaml"
}

func (a *VersionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(a, err, additionalContext...)
}
