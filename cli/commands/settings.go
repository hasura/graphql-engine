package commands

import (
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewSettingsCmd returns the version command
func NewSettingsCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &settingsOptions{
		EC: ec,
	}
	settingsCmd := &cobra.Command{
		Use:   "settings",
		Short: "Set and get settings",
		Long:  "Set or retreive a hasura setting in the database.",
		Example: `  # Get the current migration_mode setting:
  hasura settings --get migration_mode

  # Set migration_mode to false:
  hasura settings --set migration_mode=false`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}
	f := settingsCmd.Flags()
	f.StringVar(&opts.getKey, "get", "", "get setting")
	f.StringVar(&opts.setKeyValue, "set", "", "set setting")
	return settingsCmd
}

type settingsOptions struct {
	EC *cli.ExecutionContext

	getKey      string
	setKeyValue string
}

func (o *settingsOptions) run() error {
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

	if o.setKeyValue != "" {
		arr := strings.Split(o.setKeyValue, "=")
		if len(arr) != 2 {
			o.EC.Logger.WithError(err).Errorf("`set` must be in format `key=value`")
			return err
		}
		key, value := arr[0], arr[1]
		err = migrateDrv.UpdateSetting(key, value)
		if err != nil {
			o.EC.Logger.WithError(err).Errorf("cannot set value `%s` for key `%s`", key, value)
			return err
		}
		o.EC.Logger.Info("SUCCESS")
	}
	return nil
}
