package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewMetadataCmd(ec *cli.ExecutionContext) *cobra.Command {
	metdataCmd := &cobra.Command{
		Use:          "metadata",
		Short:        "Manage Hausra GraphQL Engine metdata saved in the database",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Validate()
		},
	}
	metdataCmd.AddCommand(
		NewMetadataExportCmd(ec),
		NewMetadataResetCmd(ec),
		NewMetadataApplyCmd(ec),
	)
	return metdataCmd
}
