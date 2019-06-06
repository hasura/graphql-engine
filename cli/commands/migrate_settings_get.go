package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// newMigrateSettingsGetCmd returns the version command
func newMigrateSettingsGetCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &settingsGetOptions{
		EC: ec,
	}
	migrateSettingsGetCmd := &cobra.Command{
		Use:   "get [setting-name]",
		Short: "get value of named setting",
		Example: `  # Get the current migration_mode setting:
  hasura migration settings get migration_mode`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.getKey = args[0]
			return opts.run()
		},
	}

	f := migrateSettingsGetCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	return migrateSettingsGetCmd
}

type settingsGetOptions struct {
	EC *cli.ExecutionContext

	getKey string
}

func (o *settingsGetOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version)
	if err != nil {
		o.EC.Logger.WithError(err).Errorf("cannot create migrate instance")
		return err
	}

	if o.getKey != "" {
		setting, err := migrateDrv.GetSetting(o.getKey)
		if err != nil {
			o.EC.Logger.WithError(err).Errorf("cannot get value for key `%s`", o.getKey)
			return err
		}
		o.EC.Logger.Info(setting)
	}

	return nil
}
