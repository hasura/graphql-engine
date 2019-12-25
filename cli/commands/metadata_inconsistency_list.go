package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataInconsistencyListCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataGetInconsistencyOptions{
		EC:         ec,
		actionType: "get_inconsistent",
	}

	metadataInconsistencyListCmd := &cobra.Command{
		Use:          "list",
		Short:        "list all inconsistent objects from the metadata",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			err := opts.run()
			if err != nil {
				return errors.Wrap(err, "failed to list inconsistent metadata")
			}
			return nil
		},
	}

	f := metadataInconsistencyListCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataInconsistencyListCmd
}

type metadataGetInconsistencyOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataGetInconsistencyOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}
	err = executeMetadata(o.actionType, migrateDrv, o.EC)
	if err != nil {
		return errors.Wrap(err, "Cannot reload metadata")
	}
	return nil
}
