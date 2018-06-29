package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func NewMetadataCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	metadataCmd := &cobra.Command{
		Use:          "metadata",
		Short:        "Manage Hausra GraphQL Engine metdata saved in the database",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
	}
	metadataCmd.AddCommand(
		newMetadataExportCmd(ec),
		newMetadataResetCmd(ec),
		newMetadataApplyCmd(ec),
	)
	f := metadataCmd.PersistentFlags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("access_key", f.Lookup("access-key"))
	return metadataCmd
}
