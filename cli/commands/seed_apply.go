package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli"
	migrate "github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newSeedApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &seedApplyOptions{
		EC: ec,
	}
	seedApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply seeds on the database",
		Example: `  # Apply all seeds
  hasura seed apply

  # Use with admin secret:
  hasura seed apply --admin-secret "<admin-secret>"

  # Apply seeds on another Hasura instance:
  hasura seed apply --endpoint "<endpoint>"

  # Mark seeds as applied on the server and skip execution:
  hasura seed apply --skip-execution

  # Apply a seed version only:
  hasura seed apply --version "<version>"

  # Apply last 2 down seeds:
  hasura seed apply --down 2

  # Apply only 2 up seeds:
  hasura seed apply --up 2

  # Apply only a particular version
  hasura seed apply --type up --version "<version>"

  # Rollback a particular version:
  hasura seed apply --type down --version "<version>"

  # Rollback all seeds:
  hasura seed apply --down all`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Applying seeds...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
			}
			opts.EC.Logger.Info("seeds applied")
			return nil
		},
	}
	f := seedApplyCmd.Flags()

	f.StringVar(&opts.upMigration, "up", "", "apply all or N up seed steps")
	f.StringVar(&opts.downMigration, "down", "", "apply all or N down seed steps")
	f.StringVar(&opts.versionMigration, "version", "", "only apply this particular seed")
	f.StringVar(&opts.migrationType, "type", "up", "type of seed (up, down) to be used with version flag")
	f.BoolVar(&opts.skipExecution, "skip-execution", false, "skip executing the seed action, but mark them as applied")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))
	return seedApplyCmd
}

type seedApplyOptions struct {
	EC *cli.ExecutionContext

	upMigration      string
	downMigration    string
	versionMigration string
	migrationType    string
	skipExecution    bool
}

func (o *seedApplyOptions) run() error {
	migrationType, step, err := getMigrationTypeAndStep(o.upMigration, o.downMigration, o.versionMigration, o.migrationType, o.skipExecution)
	if err != nil {
		return errors.Wrap(err, "error validating flags")
	}

	migrateDrv, err := newMigrate(o.EC.SeedDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}
	migrateDrv.SkipExecution = o.skipExecution

	err = ExecuteMigration(migrationType, migrateDrv, step)
	if err != nil {
		if err == migrate.ErrNoChange {
			o.EC.Logger.Info("nothing to apply")
			return nil
		}
		if e, ok := err.(*os.PathError); ok {
			// If Op is first, then log No migrations to apply
			if e.Op == "first" {
				o.EC.Logger.Info("No seeds to apply")
				return nil
			}
		}
		return errors.Wrap(err, "apply failed")
	}
	return nil
}
