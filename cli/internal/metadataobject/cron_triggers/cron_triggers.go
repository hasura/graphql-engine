package crontriggers

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2/version"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
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
	var op errors.Op = "crontriggers.CronTriggers.CreateFiles"
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(c.MetadataDir, c.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (c *CronTriggers) Build() (map[string]interface{}, error) {
	var op errors.Op = "crontriggers.CronTriggers.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(c.MetadataDir, c.Filename()))
	if err != nil {
		return nil, errors.E(op, c.error(err))
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, c.error(err))
	}
	return map[string]interface{}{c.Key(): obj}, nil
}

func (c *CronTriggers) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "crontriggers.CronTriggers.Export"
	b, err := metadataobject.DefaultExport(c, metadata, c.error, metadataobject.DefaultObjectTypeSequence)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return b, nil
}

func (c *CronTriggers) Key() string {
	return metadataobject.CronTriggersKey
}

func (c *CronTriggers) Filename() string {
	return "cron_triggers.yaml"
}

func (c *CronTriggers) GetFiles() ([]string, error) {
	var op errors.Op = "crontriggers.CronTriggers.GetFiles"
	rootFile := filepath.Join(c.BaseDirectory(), c.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, c.error(err))
	}
	return files, nil
}

func (c *CronTriggers) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "crontriggers.CronTriggers.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: c, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, c.error(err))
	}
	return nil
}

func (c *CronTriggers) BaseDirectory() string {
	return c.MetadataDir
}

func (c *CronTriggers) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(c, err, additionalContext...)
}
