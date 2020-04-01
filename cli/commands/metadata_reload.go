package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataReloadCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataReloadOptions{
		EC:         ec,
		actionType: "reload",
	}

	metadataReloadCmd := &cobra.Command{
		Use:   "reload",
		Short: "Reload Hasura GraphQL Engine metadata on the database",
		Example: `  # Reload all the metadata information from database:
  hasura metadata reload

  # Use with admin secret:
  hasura metadata reload --admin-secret "<admin-secret>"

  # Reload metadata on a different instance:
  hasura metadata export --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Reloading metadata...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to reload metadata")
			}
			opts.EC.Logger.Info("Metadata reloaded")
			return nil
		},
	}

	return metadataReloadCmd
}

type metadataReloadOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataReloadOptions) run() error {
	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	err = executeMetadata(o.actionType, migrateDrv, o.EC)
	if err != nil {
		return errors.Wrap(err, "Cannot reload metadata")
	}
	return nil
}
