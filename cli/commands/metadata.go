package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewMetadataCmd(ec *cli.ExecutionContext) *cobra.Command {
	metadataCmd := &cobra.Command{
		Use:          "metadata",
		Short:        "Manage Hasura GraphQL Engine metadata saved in the database",
		SilenceUsage: true,
	}
	metadataCmd.AddCommand(
		newMetadataExportCmd(ec),
		newMetadataClearCmd(ec),
		newMetadataReloadCmd(ec),
		newMetadataApplyCmd(ec),
		newMetadataTrackCmd(ec),
		newMetadataUnTrackCmd(ec),
	)
	return metadataCmd
}
