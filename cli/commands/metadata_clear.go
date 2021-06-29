package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataClearCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataClearOptions{
		EC: ec,
	}

	metadataResetCmd := &cobra.Command{
		Use:     "clear",
		Aliases: []string{"reset"},
		Short:   "Clear Hasura GraphQL engine metadata on the database",
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
}

func (o *MetadataClearOptions) Run() error {

	var err error
	metadataHandler := metadataobject.NewHandlerFromEC(o.EC)
	err = metadataHandler.ResetMetadata()
	if err != nil {
		return errors.Wrap(err, "cannot clear Metadata")
	}
	return nil
}
