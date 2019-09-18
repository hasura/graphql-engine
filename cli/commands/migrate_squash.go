package commands

import (
	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMigrateSquashCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateSquashOptions{
		EC: ec,
	}
	migrateSquashCmd := &cobra.Command{
		Use:          "squash",
		Short:        "Display current status of migrations on a database",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Squashing migrations...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
			}
			return nil
		},
	}

	f := migrateSquashCmd.Flags()
	f.Uint64Var(&opts.version, "version", 0, "squash from this version number")
	f.StringVar(&opts.name, "name", "default_squash", "name of the migration")
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return migrateSquashCmd
}

type migrateSquashOptions struct {
	EC *cli.ExecutionContext

	version uint64
	name    string
}

func (o *migrateSquashOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}
	up, down, err := migrateDrv.Squash(o.version)
	if err != nil {
		return errors.Wrap(err, "cannot squash migration")
	}

	byteUp, err := yaml.Marshal(up)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshall up query")
	}

	byteDown, err := yaml.Marshal(down)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshall down query")
	}
	timestamp := getTime()
	createOptions := mig.New(timestamp, o.name, o.EC.MigrationDir)
	createOptions.MetaUp = byteUp
	createOptions.MetaDown = byteDown

	err = createOptions.Create()
	if err != nil {
		return errors.Wrap(err, "cannot create migration")
	}
	o.EC.Logger.WithFields(log.Fields{
		"version": timestamp,
		"name":    o.name,
	}).Info("Migrations files created")
	return nil
}
