package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func newMetadataInconsistencyCmd(ec *cli.ExecutionContext) *cobra.Command {
	metadataInconsistencyCmd := &cobra.Command{
		Use:          "inconsistency",
		Short:        "Manage inconsistent objects in Hasura Metadata",
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
