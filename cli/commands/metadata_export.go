package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataExportCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataExportOptions{
		EC:         ec,
		actionType: "export",
	}

	metadataExportCmd := &cobra.Command{
		Use:   "export",
		Short: "Export Hasura GraphQL Engine metadata from the database",
		Example: `  # Export metadata and save it in metadata.yaml file:
  hasura metadata export`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := metadataExportCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataExportCmd
}

type metadataExportOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataExportOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.Config.ParsedEndpoint, o.EC.Config.AccessKey, o.EC.Logger)
	if err != nil {
		return err
	}
	return executeMetadata(o.actionType, migrateDrv, o.EC.MetadataFile)
}
