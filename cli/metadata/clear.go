package metadata

import (
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
)

func (c *config) Clear() error {
	migrateDrv, err := util.NewMigrate(c.sourceDir, c.hasuraDBConfig.endpoint, c.hasuraDBConfig.adminSecret, c.Logger, c.hasuraDBConfig.version)
	if err != nil {
		return err
	}

	err = migrateDrv.ResetMetadata()
	if err != nil {
		return errors.Wrap(err, "cannot clear Metadata")
	}
	return nil
}
