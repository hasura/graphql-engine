package allowlist

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "allow_list.yaml"
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
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *AllowListConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, fileName))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key: "allowlist",
	}
	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return err
	}
	if len(obj) != 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (a *AllowListConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var allowList interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "allowlist" {
			continue
		}
		allowList = item.Value
	}
	if allowList == nil {
		allowList = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(allowList)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(a.MetadataDir, fileName): data,
	}, nil
}

func (a *AllowListConfig) Name() string {
	return "allowlist"
}
