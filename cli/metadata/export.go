package metadata

import (
	"encoding/json"
	"io/ioutil"

	"github.com/ghodss/yaml"
	"github.com/pkg/errors"
)

func (c *config) Export() error {
	err := c.createMigrateInstance()
	if err != nil {
		return err
	}

	metaData, err := c.migrateDrv.ExportMetadata()
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

	err = ioutil.WriteFile(c.metadataPath, data, 0644)
	if err != nil {
		return errors.Wrap(err, "cannot save metadata")
	}
	return nil
}
