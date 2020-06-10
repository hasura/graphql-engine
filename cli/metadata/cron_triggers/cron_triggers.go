package crontriggers

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

const (
	fileName    string = "cron_triggers.yaml"
	metadataKey        = "cron_triggers"
)

type CronTriggers struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *CronTriggers {
	return &CronTriggers{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (f *CronTriggers) Validate() error {
	return nil
}

func (f *CronTriggers) CreateFiles() error {
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

func (f *CronTriggers) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(f.MetadataDir, fileName))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key:   metadataKey,
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return err
	}
	*metadata = append(*metadata, item)
	return nil
}

func (f *CronTriggers) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var cronTriggers interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != metadataKey {
			continue
		}
		cronTriggers = item.Value
	}
	if cronTriggers == nil {
		cronTriggers = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(cronTriggers)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(f.MetadataDir, fileName): data,
	}, nil
}

func (f *CronTriggers) Name() string {
	return metadataKey
}
