package metadata

import (
	"github.com/pkg/errors"
)

func (c *config) Clear() error {
	err := c.createMigrateInstance()
	if err != nil {
		return err
	}

	err = c.migrateDrv.ResetMetadata()
	if err != nil {
		return errors.Wrap(err, "cannot clear Metadata")
	}
	return nil
}
