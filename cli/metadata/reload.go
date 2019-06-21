package metadata

import (
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
)

func (c *config) Reload() error {
	migrateDrv, err := util.NewMigrate(c.sourceDir, c.hasuraDBConfig.endpoint, c.hasuraDBConfig.adminSecret, c.Logger, c.hasuraDBConfig.version)
	if err != nil {
		return err
	}

	err = migrateDrv.ReloadMetadata()
	if err != nil {
		return errors.Wrap(err, "cannot reload Metadata")
	}
	return nil
}
