package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/hasura/graphql-engine/cli/v2"
	migrate "github.com/hasura/graphql-engine/cli/v2/migrate"
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
  hasura migrate apply --skip-execution --version "<version>"

  # Mark migrations as applied on the server and skip execution:
  hasura migrate apply --skip-execution --up all

  # Mark migrations as rollbacked on the server and skip execution:
  hasura migrate apply --skip-execution --down all

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
			return validateConfigV3Flags(cmd, ec)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.Run()
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

	f.BoolVar(&opts.DryRun, "dry-run", false, "print the names of migrations which are going to be applied")
	f.BoolVar(&opts.AllDatabases, "all-databases", false, "set this flag to attempt to apply migrations on all databases present on server")
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
	DryRun        bool
	Source        cli.Source
	AllDatabases  bool
}

func (o *MigrateApplyOptions) Validate() error {
	if o.EC.Config.Version == cli.V2 {
		o.Source.Kind = hasura.SourceKindPG
		o.Source.Name = ""
	}

	if o.EC.Config.Version >= cli.V3 {
		if !o.AllDatabases && len(o.Source.Name) == 0 {
			return fmt.Errorf("unable to determine database on which migration should be applied")
		}
		if !o.AllDatabases {
			if len(o.Source.Name) == 0 {
				return fmt.Errorf("empty database name")
			}
			if len(o.Source.Kind) == 0 {
				// find out the database kind by making a API call to server
				// and update ec to include the database name and kind
				sourceKind, err := metadatautil.GetSourceKind(o.EC.APIClient.V1Metadata.ExportMetadata, o.Source.Name)
				if err != nil {
					return fmt.Errorf("determining database kind of %s: %w", o.Source.Name, err)
				}
				if sourceKind == nil {
					return fmt.Errorf("error determining database kind for %s, check if database exists on hasura", o.Source.Name)
				}
				o.Source.Kind = *sourceKind
			}
		}
	}
	return nil
}

type errDatabaseMigrationDirectoryNotFound struct {
	message string
}

func (e *errDatabaseMigrationDirectoryNotFound) Error() string {
	return e.message
}
func (o *MigrateApplyOptions) Run() error {
	if len(o.Source.Name) == 0 {
		o.Source = o.EC.Source
	}
	if err := o.Validate(); err != nil {
		return err
	}
	if o.DryRun && o.SkipExecution {
		return errors.New("both --skip-execution and --dry-run flags cannot be used together")
	}
	if o.AllDatabases && o.EC.Config.Version >= cli.V3 {
		o.EC.Spin("getting lists of databases from server ")
		sourcesAndKind, err := metadatautil.GetSourcesAndKind(o.EC.APIClient.V1Metadata.ExportMetadata)
		o.EC.Spinner.Stop()
		if err != nil {
			return fmt.Errorf("determing list of connected sources and kind: %w", err)
		}
		for _, source := range sourcesAndKind {
			o.Source.Kind = source.Kind
			o.Source.Name = source.Name
			if !o.DryRun {
				o.EC.Spin(fmt.Sprintf("Applying migrations on database: %s ", o.Source.Name))
			}
			err := o.Exec()
			o.EC.Spinner.Stop()
			if err != nil {
				if err == migrate.ErrNoChange {
					o.EC.Logger.Infof("nothing to apply on database: %s", o.Source.Name)
					continue
				}
				if e, ok := err.(*os.PathError); ok {
					// If Op is first, then log No migrations to apply
					if e.Op == "first" {
						o.EC.Logger.Infof("nothing to apply on database: %s", o.Source.Name)
						continue
					}
				}
				// check if the returned error is a directory not found error
				// ie might be because  a migrations/<source_name> directory is not found
				// if so skip this
				if e, ok := err.(*errDatabaseMigrationDirectoryNotFound); ok {
					o.EC.Logger.Errorf("skipping applying migrations for database %s, encountered: \n%s", o.Source.Name, e.Error())
					continue
				}
				o.EC.Logger.Errorf("skipping applying migrations for database %s, encountered: \n%v", o.Source.Name, err)
				continue
			}
			o.EC.Logger.Infof("applied migrations on database: %s", o.Source.Name)
		}
	} else {
		if !o.DryRun {
			o.EC.Spin("Applying migrations...")
		}
		err := o.Exec()
		o.EC.Spinner.Stop()
		if err != nil {
			if err == migrate.ErrNoChange {
				o.EC.Logger.Info("nothing to apply")
				return nil
			}
			// check if the returned error is a directory not found error
			// ie might be because  a migrations/<source_name> directory is not found
			// if so skip this
			if e, ok := err.(*errDatabaseMigrationDirectoryNotFound); ok {
				return fmt.Errorf("applying migrations on database %s: %w", o.Source.Name, e)
			}
			if e, ok := err.(*os.PathError); ok {
				// If Op is first, then log No migrations to apply
				if e.Op == "first" {
					o.EC.Logger.Info("nothing to apply")
					return nil
				}
			}
			return fmt.Errorf("apply failed\n%w", err)
		}
		if !o.DryRun {
			o.EC.Logger.Info("migrations applied")
		}
	}
	return nil
}
func (o *MigrateApplyOptions) Exec() error {
	if o.EC.Config.Version >= cli.V3 {
		// check if  a migrations directory exists for source in project
		migrationDirectory := filepath.Join(o.EC.MigrationDir, o.Source.Name)
		if f, err := os.Stat(migrationDirectory); err != nil || f == nil {
			return &errDatabaseMigrationDirectoryNotFound{fmt.Sprintf("expected to find a migrations directory for database %s in %s, but encountered error: %s", o.Source.Name, o.EC.MigrationDir, err.Error())}
		}
	}
	if o.AllDatabases && (len(o.GotoVersion) > 0 || len(o.VersionMigration) > 0) {
		return fmt.Errorf("cannot use --goto or --version in conjunction with --all-databases")
	}
	migrationType, step, err := getMigrationTypeAndStep(o.UpMigration, o.DownMigration, o.VersionMigration, o.MigrationType, o.GotoVersion, o.SkipExecution)
	if err != nil {
		return errors.Wrap(err, "error validating flags")
	}

	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return err
	}
	migrateDrv.SkipExecution = o.SkipExecution
	migrateDrv.DryRun = o.DryRun

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
		return "", 0, errors.New("only one migration type can be applied at a time (--up, --down, --type or --goto)")
	}

	skipExecutionValid := migrationName == "version" || migrationName == "up" || migrationName == "down"
	if !skipExecutionValid && skipExecution {
		return "", 0, errors.New("--skip-execution flag can be set only with --version, --up, --down flags")
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
