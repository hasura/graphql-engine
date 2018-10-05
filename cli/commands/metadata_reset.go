package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataResetCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataResetOptions{
		EC:         ec,
		actionType: "reset",
	}

	metadataResetCmd := &cobra.Command{
		Use:   "reset",
		Short: "Reset or clean Hasura GraphQL Engine metadata on the database",
		Example: `  # Clean all the metadata information from database:
  hasura metadata reset`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := metadataResetCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataResetCmd
}

type metadataResetOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataResetOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.Config.ParsedEndpoint, o.EC.Config.AccessKey, o.EC.Logger)
	if err != nil {
		return err
	}
	err = executeMetadata(o.actionType, migrateDrv, o.EC.MetadataFile)
	if err != nil {
		return errors.Wrap(err, "Cannot reset metadata")
	}
	return nil
}
