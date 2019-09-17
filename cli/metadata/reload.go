package metadata

import (
	"github.com/pkg/errors"
)

func (c *config) Reload() error {
	err := c.createMigrateInstance()
	if err != nil {
		return err
	}

	err = c.migrateDrv.ReloadMetadata()
	if err != nil {
		return errors.Wrap(err, "cannot reload Metadata")
	}
	return nil
}
