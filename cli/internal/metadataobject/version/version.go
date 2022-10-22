package version

import (
	"bytes"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
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

func (a *VersionConfig) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, a.Filename()))
	if err != nil {
		return nil, a.error(err)
	}
	var v Version
	err = yaml.Unmarshal(data, &v)
	if err != nil {
		return nil, a.error(err)
	}
	return map[string]interface{}{a.Key(): v.Version}, nil
}

func (a *VersionConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var version map[string]yaml.Node
	if v, ok := metadata[a.Key()]; ok {
		version = map[string]yaml.Node{a.Key(): v}
	} else {
		return nil, nil
	}
	var buf bytes.Buffer
	err := metadataobject.GetEncoder(&buf).Encode(version)
	if err != nil {
		return nil, a.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(a.MetadataDir, a.Filename())): buf.Bytes(),
	}, nil
}

func (a *VersionConfig) Key() string {
	return metadataobject.VersionKey
}

func (a *VersionConfig) Filename() string {
	return "version.yaml"
}

func (a *VersionConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(a.BaseDirectory(), a.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, a.error(err)
	}
	return files, nil
}

func (a *VersionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: a, WriteDiffOpts: opts})
	if err != nil {
		return a.error(err)
	}
	return nil
}

func (a *VersionConfig) BaseDirectory() string {
	return a.MetadataDir
}

func (a *VersionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(a, err, additionalContext...)
}
