package commands

import (
	"os"
	"strconv"

	"github.com/hasura/graphql-engine/cli"
	migrate "github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMigrateApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MigrateApplyOptions{
		EC: ec,
	}
	migrateApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply migrations on the database",
		Example: `  # Apply all migrations
  hasura migrate apply

  # Use with admin secret:
  hasura migrate apply --admin-secret "<admin-secret>"

  # Apply migrations on another Hasura instance:
  hasura migrate apply --endpoint "<endpoint>"

  # Mark migration as applied on the server and skip execution:
  hasura migrate apply --skip-execution

  # Apply a particular migration version only:
  hasura migrate apply --version "<version>"

  # Apply last 2 down migrations:
  hasura migrate apply --down 2

  # Apply only 2 up migrations:
  hasura migrate apply --up 2

  # Apply only a particular version
  hasura migrate apply --type up --version "<version>"
  
  # Apply all up migrations upto version 125, last applied is 100
  hasura migrate apply --goto 125
  
  # Apply all down migrations upto version 125, last applied is 150
  hasura migrate apply --goto 125

  # Rollback a particular version:
  hasura migrate apply --type down --version "<version>"

  # Rollback all migrations:
  hasura migrate apply --down all`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Applying migrations...")
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				if err == migrate.ErrNoChange {
					opts.EC.Logger.Info("nothing to apply")
					return nil
				}
				if e, ok := err.(*os.PathError); ok {
					// If Op is first, then log No migrations to apply
					if e.Op == "first" {
						opts.EC.Logger.Info("nothing to apply")
						return nil
					}
				}
				return errors.Wrap(err, "apply failed")
			}
			opts.EC.Logger.Info("migrations applied")
			return nil
		},
	}
	f := migrateApplyCmd.Flags()
	f.SortFlags = false

	f.StringVar(&opts.UpMigration, "up", "", "apply all or N up migration steps")
	f.StringVar(&opts.DownMigration, "down", "", "apply all or N down migration steps")
	f.StringVar(&opts.GotoVersion, "goto", "", "apply migration chain up to to the version specified")

	f.StringVar(&opts.VersionMigration, "version", "", "only apply this particular migration")
	f.BoolVar(&opts.SkipExecution, "skip-execution", false, "skip executing the migration action, but mark them as applied")
	f.StringVar(&opts.MigrationType, "type", "up", "type of migration (up, down) to be used with version flag")

	return migrateApplyCmd
}

type MigrateApplyOptions struct {
	EC *cli.ExecutionContext

	UpMigration      string
	DownMigration    string
	VersionMigration string
	MigrationType    string
	// version up to which migration chain has to be applied
	GotoVersion   string
	SkipExecution bool
}

func (o *MigrateApplyOptions) Run() error {
	migrationType, step, err := getMigrationTypeAndStep(o.UpMigration, o.DownMigration, o.VersionMigration, o.MigrationType, o.GotoVersion, o.SkipExecution)
	if err != nil {
		return errors.Wrap(err, "error validating flags")
	}

	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	migrateDrv.SkipExecution = o.SkipExecution

	return ExecuteMigration(migrationType, migrateDrv, step)
}

// Only one flag out of up, down and version can be set at a time. This function
// checks whether that is the case and returns an error is not
func getMigrationTypeAndStep(upMigration, downMigration, versionMigration, migrationType, gotoVersion string, skipExecution bool) (string, int64, error) {
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
	if gotoVersion != "" {
		migrationName = "gotoVersion"
		stepString = gotoVersion
		flagCount++
	}

	if flagCount > 1 {
		return "", 0, errors.New("only one migration type can be applied at a time (--up, --down or --goto)")
	}

	if migrationName != "version" && skipExecution {
		return "", 0, errors.New("--skip-execution flag can be set only with --version flag")
	}

	if stepString == "all" && migrationName != "version" {
		return migrationName, -1, nil
	}

	step, err := strconv.ParseInt(stepString, 10, 64)
	if err != nil {
		return "", 0, errors.Wrap(err, "not a valid input for steps/version")
	}
	return migrationName, step, nil
}
