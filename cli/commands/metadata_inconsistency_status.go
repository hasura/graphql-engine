package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/spf13/cobra"
)

func newMetadataInconsistencyStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataInconsistencyListOptions{
		EC: ec,
	}

	metadataInconsistencyStatusCmd := &cobra.Command{
		Use:          "status",
		Short:        "Check if the Hasura Metadata is inconsistent or not",
		Long:         "At times, when developing, the Hasura Metadata can become inconsistent. This command can be used to check if the Metadata is inconsistent or not.",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.EC.Spin("reading metadata status...")
			err := opts.read(projectmetadata.NewHandlerFromEC(ec))
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.E(op, fmt.Errorf("failed to read metadata status: %w", err))
			}
			if opts.isConsistent {
				opts.EC.Logger.Println("metadata is consistent")
			} else {
				return errors.E(op, "metadata is inconsistent, use 'hasura metadata ic list' command to see the inconsistent objects")
			}
			return nil
		},
	}

	return metadataInconsistencyStatusCmd
}
