package commands

import (
	"github.com/hasura/graphql-engine/cli"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
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
		Short:        "",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Squashing migrations...")
			opts.version = getTime()
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
			}
			opts.EC.Logger.WithFields(log.Fields{
				"version": opts.version,
				"name":    opts.name,
			}).Info("Migrations files created")
			return nil
		},
	}

	f := migrateSquashCmd.Flags()
	f.Uint64Var(&opts.from, "from", 0, "squash from this version number")
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

	from    uint64
	name    string
	version int64
}

func (o *migrateSquashOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}
	return mig.SquashCmd(migrateDrv, o.from, o.version, o.name, o.EC.MigrationDir)
}
