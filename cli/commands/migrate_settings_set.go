package commands

import (
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// newMigrateSettingsSetCmd returns the version command
func newMigrateSettingsSetCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &settingsSetOptions{
		EC: ec,
	}
	migrateSettingsSetCmd := &cobra.Command{
		Use:   "set [setting-name]=[new-value]",
		Short: "Update migration setting value",
		Example: `  # Set migration_mode to false:
  hasura migration settings set migration_mode=false`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			arr := strings.Split(args[0], "=")
			if len(arr) != 2 {
				return errors.New("set must be in format `key=value`")
			}
			opts.key, opts.value = arr[0], arr[1]
			return opts.run()
		},
	}
	return migrateSettingsSetCmd
}

type settingsSetOptions struct {
	EC *cli.ExecutionContext

	key   string
	value string
}

func (o *settingsSetOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version)
	if err != nil {
		o.EC.Logger.WithError(err).Errorf("cannot create migrate instance")
		return err
	}

	err = migrateDrv.UpdateSetting(o.key, o.value)
	if err != nil {
		o.EC.Logger.WithError(err).Errorf("cannot set value `%s` for key `%s`", o.key, o.value)
		return err
	}
	o.EC.Logger.Info("SUCCESS")
	return nil
}
