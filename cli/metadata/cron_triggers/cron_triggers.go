package crontriggers

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/version"

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

	logger             *logrus.Logger
	serverFeatureFlags *version.ServerFeatureFlags
}

func New(ec *cli.ExecutionContext, baseDir string) *CronTriggers {
	return &CronTriggers{
		MetadataDir:        baseDir,
		logger:             ec.Logger,
		serverFeatureFlags: ec.Version.ServerFeatureFlags,
	}
}

func (c *CronTriggers) Validate() error {
	return nil
}

func (c *CronTriggers) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(c.MetadataDir, fileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (c *CronTriggers) Build(metadata *yaml.MapSlice) error {
	if !c.serverFeatureFlags.HasCronTriggers {
		c.logger.WithField("metadata_plugin", "cron_triggers").Warnf("Skipping building %s", fileName)
		return nil
	}
	data, err := ioutil.ReadFile(filepath.Join(c.MetadataDir, fileName))
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

func (c *CronTriggers) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	if !c.serverFeatureFlags.HasCronTriggers {
		c.logger.Debugf("Skipping creating %s", fileName)
		return make(map[string][]byte), nil
	}
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
		filepath.Join(c.MetadataDir, fileName): data,
	}, nil
}

func (c *CronTriggers) Name() string {
	return metadataKey
}
