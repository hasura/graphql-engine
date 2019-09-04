package commands

import (
	"os"
	"strconv"

	"github.com/hasura/graphql-engine/cli"
	migrate "github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMigrateApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateApplyOptions{
		EC: ec,
	}
	migrateApplyCmd := &cobra.Command{
		Use:          "apply",
		Short:        "Apply migrations on the database",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Applying migrations...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
			}
			opts.EC.Logger.Info("migrations applied")
			return nil
		},
	}
	f := migrateApplyCmd.Flags()

	f.StringVar(&opts.upMigration, "up", "", "apply all or N up migration steps")
	f.StringVar(&opts.downMigration, "down", "", "apply all or N down migration steps")
	f.StringVar(&opts.versionMigration, "version", "", "only apply this particular migration")
	f.StringVar(&opts.migrationType, "type", "up", "type of migration (up, down) to be used with version flag")
	f.BoolVar(&opts.skipExecution, "skip-execution", false, "skip executing the migration action, but mark them as applied")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))
	return migrateApplyCmd
}

type migrateApplyOptions struct {
	EC *cli.ExecutionContext

	upMigration      string
	downMigration    string
	versionMigration string
	migrationType    string
	skipExecution    bool
}

func (o *migrateApplyOptions) run() error {
	migrationType, step, err := getMigrationTypeAndStep(o.upMigration, o.downMigration, o.versionMigration, o.migrationType, o.skipExecution)
	if err != nil {
		return errors.Wrap(err, "error validating flags")
	}

	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version)
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
				o.EC.Logger.Info("No migrations to apply")
				return nil
			}
		}
		return errors.Wrap(err, "apply failed")
	}
	return nil
}

// Only one flag out of up, down and version can be set at a time. This function
// checks whether that is the case and returns an error is not
func getMigrationTypeAndStep(upMigration, downMigration, versionMigration, migrationType string, skipExecution bool) (string, int64, error) {
	var flagCount = 0
	var stepString = "all"
	var migrationName = "up"
	if upMigration != "" {
		stepString = upMigration
		flagCount++
	}
	if downMigration != "" {
		migrationName = "down"
		stepString = downMigration
		flagCount++
	}
	if versionMigration != "" {
		migrationName = "version"
		stepString = versionMigration
		if migrationType == "down" {
			stepString = "-" + stepString
		}
		flagCount++
	}

	if flagCount > 1 {
		return "", 0, errors.New("Only one migration type can be applied at a time (--up, --down or --goto)")
	}

	if migrationName != "version" && skipExecution {
		return "", 0, errors.New("--skip-execution flag can be set only with --version flag")
	}

	if stepString == "all" && migrationName != "version" {
		return migrationName, -1, nil
	}

	step, err := strconv.ParseInt(stepString, 10, 64)
	if err != nil {
		return "", 0, errors.Wrap(err, "not a valid input for steps")
	}
	return migrationName, step, nil
}
