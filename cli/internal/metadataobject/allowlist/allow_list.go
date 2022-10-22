package allowlist

import (
	"bytes"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
)

type AllowListConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *AllowListConfig {
	return &AllowListConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
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
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, a.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *AllowListConfig) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, a.Filename()))
	if err != nil {
		return nil, a.error(err)
	}
	var obj []yaml.Node
	err = yaml.NewDecoder(bytes.NewReader(data)).Decode(&obj)
	if err != nil {
		return nil, a.error(err)
	}
	return map[string]interface{}{a.Key(): obj}, nil
}

func (a *AllowListConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	return metadataobject.DefaultExport(a, metadata, a.error, metadataobject.DefaultObjectTypeSequence)
}

func (a *AllowListConfig) Key() string {
	return metadataobject.AllowListKey
}

func (a *AllowListConfig) Filename() string {
	return "allow_list.yaml"
}

func (a *AllowListConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(a.BaseDirectory(), a.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, a.error(err)
	}
	return files, nil
}

func (a *AllowListConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: a, WriteDiffOpts: opts})
	if err != nil {
		return a.error(err)
	}
	return nil
}

func (a *AllowListConfig) BaseDirectory() string {
	return a.MetadataDir
}

func (a *AllowListConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(a, err, additionalContext...)
}
