package commands

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2"
	herrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	migrate "github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/spf13/cobra"
)

func newMigrateApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MigrateApplyOptions{
		EC: ec,
	}
	migrateApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply migrations on the database",
		Long: `Migrations represent the modifications needed to reach the desired state of a database schema. Running this command will apply the migrations on the database.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-migrations/
`,
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
			op := genOpName(cmd, "PreRunE")
			if err := validateConfigV3FlagsWithAll(cmd, ec); err != nil {
				return herrors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if err := opts.Run(); err != nil {
				return herrors.E(op, err)
			}
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

	f.BoolVar(&opts.DryRun, "dry-run", false, "print the names of migrations which are going to be applied")
	f.BoolVar(&opts.EC.AllDatabases, "all-databases", false, "set this flag to attempt to apply migrations on all databases present on server")
	f.BoolVar(&opts.ProgressBarLogs, "progressbar-logs", false, "print the logs of progressbar")
	if err := f.MarkHidden("progressbar-logs"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	return migrateApplyCmd
}

type MigrateApplyOptions struct {
	EC *cli.ExecutionContext

	UpMigration      string
	DownMigration    string
	VersionMigration string
	MigrationType    string
	// version up to which migration chain has to be applied
	GotoVersion     string
	SkipExecution   bool
	DryRun          bool
	Source          cli.Source
	ProgressBarLogs bool
}

func (o *MigrateApplyOptions) Validate() error {
	var op herrors.Op = "commands.MigrateApplyOptions.Validate"
	if o.EC.Config.Version == cli.V2 {
		o.Source.Kind = hasura.SourceKindPG
		o.Source.Name = ""
	}

	if o.DryRun && o.SkipExecution {
		return herrors.E(op, "both --skip-execution and --dry-run flags cannot be used together")
	}
	if o.DryRun && o.EC.AllDatabases {
		return herrors.E(op, "both --all-databases and --dry-run flags cannot be used together")
	}

	if o.EC.Config.Version >= cli.V3 {
		if !o.EC.AllDatabases && len(o.Source.Name) == 0 {
			return herrors.E(op, "unable to determine database on which migration should be applied")
		}
		if !o.EC.AllDatabases {
			if len(o.Source.Name) == 0 {
				return herrors.E(op, "empty database name")
			}
			if len(o.Source.Kind) == 0 {
				// find out the database kind by making a API call to server
				// and update ec to include the database name and kind
				sourceKind, err := metadatautil.GetSourceKind(o.EC.APIClient.V1Metadata.ExportMetadata, o.Source.Name)
				if err != nil {
					return herrors.E(op, fmt.Errorf("determining database kind of '%s': %w", o.Source.Name, err))
				}
				if sourceKind == nil {
					return herrors.E(op, fmt.Errorf("error determining database kind for '%s', check if database exists on hasura", o.Source.Name))
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
	var op herrors.Op = "commands.MigrateApplyOptions.Run"
	results, err := o.Apply()
	if err != nil {
		return herrors.E(op, err)
	}
	var failedSources []string
	for result := range results {
		if result.Error != nil {
			failedSources = append(failedSources, result.DatabaseName)
			o.EC.Logger.Errorf("%v", result.Error)
		} else if len(result.Message) > 0 {
			o.EC.Logger.Infof(result.Message)
		}
	}
	if len(failedSources) != 0 {
		return herrors.E(op, fmt.Errorf("applying migrations failed on database(s): %s", strings.Join(failedSources, ",")))
	}
	return nil
}

type MigrateApplyResult struct {
	DatabaseName string
	Message      string
	Error        error
}

func (o *MigrateApplyOptions) Apply() (chan MigrateApplyResult, error) {
	var op herrors.Op = "commands.MigrateApplyOptions.Apply"
	resultChan := make(chan MigrateApplyResult)

	handleError := func(err error) (string, error) {
		var op herrors.Op = "commands.MigrateApplyOptions.Apply.handleError"
		if err == nil {
			return "", nil
		}
		var errPath *os.PathError
		var errNotFound *errDatabaseMigrationDirectoryNotFound

		switch {
		case errors.Is(err, migrate.ErrNoChange):
			return fmt.Sprintf("nothing to apply on database: %s", o.Source.Name), nil
		case errors.As(err, &errPath):
			// If Op is first, then log No migrations to apply
			if errPath.Op == "first" {
				return fmt.Sprintf("nothing to apply on database: %s", o.Source.Name), nil
			}
		case errors.As(err, &errNotFound):
			// check if the returned error is a directory not found error
			// ie might be because  a migrations/<source_name> directory is not found
			// if so skip this
			return "", herrors.E(op, fmt.Errorf("skipping applying migrations on database '%s', encountered: \n%s", o.Source.Name, errNotFound.Error()))
		}
		return "", herrors.E(op, fmt.Errorf("skipping applying migrations on database '%s', encountered: \n%w", o.Source.Name, err))
	}

	if len(o.Source.Name) == 0 && !o.EC.AllDatabases {
		o.Source = o.EC.Source
	}
	if err := o.Validate(); err != nil {
		return nil, herrors.E(op, err)
	}
	if o.EC.AllDatabases && o.EC.Config.Version >= cli.V3 {
		o.EC.Spin("getting lists of databases from server ")
		sourcesAndKind, err := metadatautil.GetSourcesAndKind(o.EC.APIClient.V1Metadata.ExportMetadata)
		o.EC.Spinner.Stop()
		if err != nil {
			return nil, herrors.E(op, err)
		}
		go func() {
			defer close(resultChan)
			for _, source := range sourcesAndKind {
				result := MigrateApplyResult{
					DatabaseName: source.Name,
					Message:      "",
					Error:        nil,
				}
				o.Source.Kind = source.Kind
				o.Source.Name = source.Name
				err := o.Exec()
				if err != nil {
					result.Message, result.Error = handleError(err)
				} else {
					result.Message = fmt.Sprintf("migrations applied on database: %s", o.Source.Name)
				}
				resultChan <- result
			}
		}()
	} else {
		go func() {
			defer close(resultChan)
			result := MigrateApplyResult{
				DatabaseName: o.Source.Name,
				Message:      "",
				Error:        nil,
			}
			err := o.Exec()
			if err != nil {
				result.Message, result.Error = handleError(err)
			} else {
				result.Message = "migrations applied"
			}
			resultChan <- result
		}()
	}

	return resultChan, nil
}

func (o *MigrateApplyOptions) Exec() error {
	var op herrors.Op = "commands.MigrateApplyOptions.Exec"
	if o.EC.Config.Version >= cli.V3 {
		// check if  a migrations directory exists for source in project
		migrationDirectory := filepath.Join(o.EC.MigrationDir, o.Source.Name)
		if f, err := os.Stat(migrationDirectory); err != nil || f == nil {
			return herrors.E(op, &errDatabaseMigrationDirectoryNotFound{fmt.Sprintf("expected to find a migrations directory for database '%s' in %s, but encountered error: %s", o.Source.Name, o.EC.MigrationDir, err.Error())})
		}
	}
	if o.EC.AllDatabases && (len(o.GotoVersion) > 0 || len(o.VersionMigration) > 0) {
		return herrors.E(op, "cannot use --goto or --version in conjunction with --all-databases")
	}
	migrationType, step, err := getMigrationTypeAndStep(o.UpMigration, o.DownMigration, o.VersionMigration, o.MigrationType, o.GotoVersion, o.SkipExecution)
	if err != nil {
		return herrors.E(op, fmt.Errorf("error validating flags: %w", err))
	}

	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return herrors.E(op, err)
	}
	migrateDrv.SkipExecution = o.SkipExecution
	migrateDrv.DryRun = o.DryRun
	migrateDrv.ProgressBarLogs = o.ProgressBarLogs

	if err := ExecuteMigration(migrationType, migrateDrv, step); err != nil {
		return herrors.E(op, err)
	}
	return nil
}

// Only one flag out of up, down and version can be set at a time. This function
// checks whether that is the case and returns an error is not
func getMigrationTypeAndStep(upMigration, downMigration, versionMigration, migrationType, gotoVersion string, skipExecution bool) (string, int64, error) {
	var op herrors.Op = "commands.getMigrationTypeAndStep"
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
		return "", 0, herrors.E(op, "only one migration type can be applied at a time (--up, --down, --type or --goto)")
	}

	skipExecutionValid := migrationName == "version" || migrationName == "up" || migrationName == "down"
	if !skipExecutionValid && skipExecution {
		return "", 0, herrors.E(op, "--skip-execution flag can be set only with --version, --up, --down flags")
	}

	if stepString == "all" && migrationName != "version" {
		return migrationName, -1, nil
	}

	step, err := strconv.ParseInt(stepString, 10, 64)
	if err != nil {
		return "", 0, herrors.E(op, fmt.Errorf("not a valid input for steps/version: %w", err))
	}
	return migrationName, step, nil
}
