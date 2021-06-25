package crontriggers

import (
	"io/ioutil"
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/hasura/graphql-engine/cli/v2/version"

	"github.com/hasura/graphql-engine/cli/v2"
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

func (c *CronTriggers) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(c.MetadataDir, fileName))
	if err != nil {
		return c.Error(err)
	}

	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return c.Error(err)
	}
	if len(obj) > 0 {
		item := yaml.MapItem{
			Key:   metadataKey,
			Value: obj,
		}
		*metadata = append(*metadata, item)
	}
	return nil
}

func (c *CronTriggers) Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
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
		return nil, c.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(c.MetadataDir, fileName)): data,
	}, nil
}

func (c *CronTriggers) Name() string {
	return metadataKey
}

func (c *CronTriggers) Error(err error, additionalContext ...string) errors2.ErrParsingMetadataObject {
	return errors2.NewErrParsingMetadataObject(c.Name(), fileName, additionalContext, err)
}
