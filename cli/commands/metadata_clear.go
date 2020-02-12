package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataClearCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &MetadataClearOptions{
		EC:         ec,
		ActionType: "clear",
	}

	metadataResetCmd := &cobra.Command{
		Use:     "clear",
		Aliases: []string{"reset"},
		Short:   "Clear Hasura GraphQL Engine metadata on the database",
		Example: `  # Clear all the metadata information from database:
  hasura metadata clear

  # Use with admin secret:
  hasura metadata clear --admin-secret "<admin-secret>"

  # Clear metadata on a different Hasura instance:
  hasura metadata clear --endpoint "<endpoint>"`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if cmd.CalledAs() == "reset" {
				opts.EC.Logger.Warn("metadata reset command is deprecated, use metadata clear instead")
			}
			opts.EC.Spin("Clearing metadata...")
			err := opts.Run()
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

type MetadataClearOptions struct {
	EC *cli.ExecutionContext

	ActionType string
}

func (o *MetadataClearOptions) Run() error {
	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	err = executeMetadata(o.ActionType, migrateDrv, o.EC)
	if err != nil {
		return errors.Wrap(err, "Cannot clear metadata")
	}
	return nil
}
