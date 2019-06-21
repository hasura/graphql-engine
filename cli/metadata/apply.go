package metadata

import (
	"io/ioutil"
	"os"

	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v2"
)

func (c *config) Apply() error {
	migrateDrv, err := util.NewMigrate(c.sourceDir, c.hasuraDBConfig.endpoint, c.hasuraDBConfig.adminSecret, c.Logger, c.hasuraDBConfig.version)
	if err != nil {
		return err
	}

	var data interface{}
	var metadataContent []byte
	for _, format := range []string{"yaml", "json"} {
		metadataPath, err := c.getMetadataFilePath(format)
		if err != nil {
			return errors.Wrap(err, "cannot apply metadata")
		}

		metadataContent, err = ioutil.ReadFile(metadataPath)
		if err != nil {
			if os.IsNotExist(err) {
				continue
			}
			return err
		}
		break
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
