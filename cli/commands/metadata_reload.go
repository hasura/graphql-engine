package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataReloadCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataReloadOptions{
		EC:         ec,
		actionType: "reload",
	}

	metadataReloadCmd := &cobra.Command{
		Use:   "reload",
		Short: "Reload Hasura GraphQL Engine metadata on the database",
		Example: `  # Reload all the metadata information from database:
  hasura metadata reload`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Reloading metadata...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to reload metadata")
			}
			opts.EC.Logger.Info("Metadata reloaded")
			return nil
		},
	}

	f := metadataReloadCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataReloadCmd
}

type metadataReloadOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataReloadOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version)
	if err != nil {
		return err
	}
	err = executeMetadata(o.actionType, migrateDrv, o.EC)
	if err != nil {
		return errors.Wrap(err, "Cannot reload metadata")
	}
	return nil
}
