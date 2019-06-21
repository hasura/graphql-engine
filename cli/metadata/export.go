package metadata

import (
	"encoding/json"
	"io/ioutil"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
)

func (c *config) Export() error {
	migrateDrv, err := util.NewMigrate(c.sourceDir, c.hasuraDBConfig.endpoint, c.hasuraDBConfig.adminSecret, c.Logger, c.hasuraDBConfig.version)
	if err != nil {
		return err
	}

	metaData, err := migrateDrv.ExportMetadata()
	if err != nil {
		return errors.Wrap(err, "cannot export metadata")
	}

	t, err := json.Marshal(metaData)
	if err != nil {
		return errors.Wrap(err, "cannot Marshal metadata")
	}

	data, err := yaml.JSONToYAML(t)
	if err != nil {
		return err
	}

	metadataPath, err := c.getMetadataFilePath("yaml")
	if err != nil {
		return errors.Wrap(err, "cannot save metadata")
	}

	err = ioutil.WriteFile(metadataPath, data, 0644)
	if err != nil {
		return errors.Wrap(err, "cannot save metadata")
	}
	return nil
}
