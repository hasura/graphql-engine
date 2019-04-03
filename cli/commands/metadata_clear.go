package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataClearCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataClearOptions{
		EC:         ec,
		actionType: "clear",
	}

	metadataResetCmd := &cobra.Command{
		Use:     "clear",
		Aliases: []string{"reset"},
		Short:   "Clear Hasura GraphQL Engine metadata on the database",
		Example: `  # Clear all the metadata information from database:
  hasura metadata clear`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if cmd.CalledAs() == "reset" {
				opts.EC.Logger.Warn("metadata reset command is deprecated, use metadata clear instead")
			}
			opts.EC.Spin("Clearing metadata...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to clear metadata")
			}
			opts.EC.Logger.Info("Metadata cleared")
			return nil
		},
	}

	f := metadataResetCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataResetCmd
}

type metadataClearOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataClearOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version)
	if err != nil {
		return err
	}
	err = executeMetadata(o.actionType, migrateDrv, o.EC)
	if err != nil {
		return errors.Wrap(err, "Cannot clear metadata")
	}
	return nil
}
