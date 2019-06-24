package metadata

import (
	"io/ioutil"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
)

func (c *config) Apply() error {
	migrateDrv, err := util.NewMigrate(c.sourceDir, c.hasuraDBConfig.endpoint, c.hasuraDBConfig.adminSecret, c.Logger, c.hasuraDBConfig.version)
	if err != nil {
		return err
	}

	var data interface{}

	metadataContent, err := ioutil.ReadFile(c.metadataPath)
	if err != nil {
		return err
	}

	if metadataContent == nil {
		return errors.New("Unable to locate metadata.[yaml|json] file under migrations directory")
	}

	err = yaml.Unmarshal(metadataContent, &data)
	if err != nil {
		return errors.Wrap(err, "cannot parse metadata file")
	}

	err = migrateDrv.ApplyMetadata(data)
	if err != nil {
		return errors.Wrap(err, "cannot apply metadata on the database")
	}
	return nil
}
