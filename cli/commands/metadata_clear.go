package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataClearCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataClearOptions{
		EC:         ec,
		ActionType: "clear",
	}

	metadataResetCmd := &cobra.Command{
		Use:     "clear",
		Aliases: []string{"reset"},
		Short:   "Clear Hasura GraphQL Engine metadata on the database",
		Example: `  # Clear all the metadata information from database:
  hasura metadata clear

  # Use with admin secret:
  hasura metadata clear --admin-secret "<admin-secret>"

  # Clear metadata on a different Hasura instance:
  hasura metadata clear --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			if cmd.CalledAs() == "reset" {
				opts.EC.Logger.Warn("metadata reset command is deprecated, use metadata clear instead")
			}
			opts.EC.Spin("Clearing metadata...")
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to clear metadata")
			}
			opts.EC.Logger.Info("Metadata cleared")
			return nil
		},
	}

	return metadataResetCmd
}

type MetadataClearOptions struct {
	EC *cli.ExecutionContext

	ActionType string
}

func (o *MetadataClearOptions) Run() error {
	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	err = executeMetadata(o.ActionType, migrateDrv, o.EC)
	if err != nil {
		return errors.Wrap(err, "Cannot clear metadata")
	}
	return nil
}
