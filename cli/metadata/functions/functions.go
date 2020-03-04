package functions

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

const (
	fileName string = "functions.yaml"
)

type FunctionConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *FunctionConfig {
	return &FunctionConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (f *FunctionConfig) Validate() error {
	return nil
}

func (f *FunctionConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(f.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (f *FunctionConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(f.MetadataDir, fileName))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key: "functions",
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

func (f *FunctionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var functions interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "functions" {
			continue
		}
		functions = item.Value
	}
	if functions == nil {
		functions = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(functions)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(f.MetadataDir, fileName): data,
	}, nil
}

func (f *FunctionConfig) Name() string {
	return "functions"
}
