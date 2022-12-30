package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"
	"github.com/spf13/cobra"
)

func newMetadataClearCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataClearOptions{
		EC: ec,
	}

	metadataResetCmd := &cobra.Command{
		Use:     "clear",
		Aliases: []string{"reset"},
		Short:   "Clear Hasura GraphQL Engine Metadata on the database",
		Long:    "This command allows you to clear the Hasura GraphQL Engine Metadata. Passing in the `--endpoint` flag will clear the Hasura Metadata on the HGE instance specified by the endpoint.",
		Example: `  # Clear all the metadata information from database:
  hasura metadata clear

  # Use with admin secret:
  hasura metadata clear --admin-secret "<admin-secret>"

  # Clear metadata on a different Hasura instance:
  hasura metadata clear --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if cmd.CalledAs() == "reset" {
				opts.EC.Logger.Warn("metadata reset command is deprecated, use metadata clear instead")
			}
			opts.EC.Spin("Clearing metadata...")
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.E(op, fmt.Errorf("failed to clear metadata: %w", err))
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
	var op errors.Op = "commands.MetadataClearOptions.Run"
	var err error
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	err = metadataHandler.ResetMetadata()
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot clear Metadata: %w", err))
	}
	return nil
}
