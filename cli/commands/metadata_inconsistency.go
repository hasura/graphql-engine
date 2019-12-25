package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewMetadataInconsistecyCmd(ec *cli.ExecutionContext) *cobra.Command {
	metadataInconsistencyCmd := &cobra.Command{
		Use:          "inconsistency",
		Aliases:      []string{"inconsistencies", "ic"},
		SilenceUsage: true,
	}

	metadataInconsistencyCmd.AddCommand()
	return metadataInconsistencyCmd
}
