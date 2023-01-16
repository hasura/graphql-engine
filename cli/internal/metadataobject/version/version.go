package version

import (
	"bytes"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
	var op errors.Op = "version.VersionConfig.CreateFiles"
	v := Version{
		Version: 3,
	}
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, a.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (a *VersionConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "version.VersionConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(a.MetadataDir, a.Filename()))
	if err != nil {
		return nil, errors.E(op, a.error(err))
	}
	var v Version
	err = yaml.Unmarshal(data, &v)
	if err != nil {
		return nil, errors.E(op, a.error(err))
	}
	return map[string]interface{}{a.Key(): v.Version}, nil
}

func (a *VersionConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "version.VersionConfig.Export"
	var version map[string]yaml.Node
	if v, ok := metadata[a.Key()]; ok {
		version = map[string]yaml.Node{a.Key(): v}
	} else {
		return nil, nil
	}
	var buf bytes.Buffer
	err := metadataobject.GetEncoder(&buf).Encode(version)
	if err != nil {
		return nil, errors.E(op, a.error(err))
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

func (a *VersionConfig) GetFiles() ([]string, error) {
	var op errors.Op = "version.VersionConfig.GetFiles"
	rootFile := filepath.Join(a.BaseDirectory(), a.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, a.error(err))
	}
	return files, nil
}

func (a *VersionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "version.VersionConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: a, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, a.error(err))
	}
	return nil
}

func (a *VersionConfig) BaseDirectory() string {
	return a.MetadataDir
}

func (a *VersionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(a, err, additionalContext...)
}
