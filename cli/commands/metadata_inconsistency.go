package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
)

func newMetadataInconsistencyCmd(ec *cli.ExecutionContext) *cobra.Command {
	metadataInconsistencyCmd := &cobra.Command{
		Use:          "inconsistency",
		Short:        "Manage inconsistent objects in the Hasura Metadata",
		Long:         "This command, when used with subcommands, can be used to manage inconsistent objects in Hasura metadata. Options include:\n\n `hasura metadata inconsistency list`, `hasura metadata inconsistency delete`, `hasura metadata inconsistency status`",
		Aliases:      []string{"inconsistencies", "ic"},
		SilenceUsage: true,
	}

	metadataInconsistencyCmd.AddCommand(
		newMetadataInconsistencyListCmd(ec),
		newMetadataInconsistencyDropCmd(ec),
		newMetadataInconsistencyStatusCmd(ec),
	)
	return metadataInconsistencyCmd
}
