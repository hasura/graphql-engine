package commands

import (
	"fmt"
	"path/filepath"
	"time"

	"errors"

	"github.com/hasura/graphql-engine/cli/v2"
	herrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"

	mig "github.com/hasura/graphql-engine/cli/v2/migrate/cmd"
	log "github.com/sirupsen/logrus"
)

const migrateCreateCmdExamples = `  # Setup migration files for the first time by introspecting a server:
  hasura migrate create "init" --from-server

  # Setup migration files for the first time by introspecting a server when there are multiple schemas:
  hasura migrate create "init" --from-server --schema myschema1,myschema2

  # Use with admin secret:
  hasura migrate create --admin-secret "<admin-secret>"

  # Setup migration files from an instance mentioned by the flag:
  hasura migrate create init --from-server --endpoint "<endpoint>"

  # Take pg_dump of schema and hasura metadata from server while specifying the schemas to include
  hasura migrate create init --from-server --schema myschema1,myschema2

  # Create up and down SQL migrations, providing contents as flags
  hasura migrate create migration-name --up-sql "CREATE TABLE article(id serial NOT NULL, title text NOT NULL, content text NOT NULL);"  --down-sql "DROP TABLE article;"
`

func newMigrateCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &migrateCreateOptions{
		EC: ec,
	}

	migrateCreateCmd := &cobra.Command{
		Use:   "create [migration-name]",
		Short: "Create ``sql`` files required for a migration",
		Long: `As you make changes to your database's schema, Hasura tracks these changes via migrations. This command creates a ` + "``sql``" + ` file that contains the changes you made that can then be applied to a database.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-migrations/
`,
		Example:      migrateCreateCmdExamples,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if cmd.Flags().Changed("up-sql") {
				opts.upSQLChanged = true
			}
			if cmd.Flags().Changed("down-sql") {
				opts.downSQLChanged = true
			}
			if cmd.Flags().Changed("metadata-from-server") {
				return herrors.E(op, "metadata-from-server flag is deprecated")
			}
			if cmd.Flags().Changed("metadata-from-file") {
				return herrors.E(op, "metadata-from-file flag is deprecated")
			}
			if err := validateConfigV3Flags(cmd, ec); err != nil {
				if errors.Is(err, errDatabaseNotFound) {
					// this means provided database is not yet connected to hasura
					// this can be ignored for `migrate create`
					// we can allow users to create migration files for databases
					// which are not connected
					ec.Logger.Warnf("database '%s' is not connected to hasura", ec.Source.Name)
					ec.Source.Kind = hasura.SourceKindPG // the default kind is postgres
					return nil
				}
				return herrors.E(op, err)
			}

			if opts.upSQLChanged && !opts.downSQLChanged {
				ec.Logger.Warn("you are creating an up migration without a down migration")
			}

			if opts.downSQLChanged && !opts.upSQLChanged {
				ec.Logger.Warn("you are creating a down migration without an up migration")
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.name = args[0]
			opts.EC.Spin("Creating migration files...")
			opts.Source = ec.Source
			version, err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return herrors.E(op, err)
			}
			opts.EC.Logger.WithFields(log.Fields{
				"version": version,
				"name":    opts.name,
			}).Info("Migrations files created")
			return nil
		},
	}
	f := migrateCreateCmd.Flags()
	opts.flags = f
	f.SortFlags = false

	f.BoolVar(&opts.fromServer, "from-server", false, "take pg_dump of schema (default: public) and Hasura metadata from the server")
	f.StringVar(&opts.sqlFile, "sql-from-file", "", "path to an SQL file which contains the SQL statements")
	f.BoolVar(&opts.sqlServer, "sql-from-server", false, "take pg_dump from the server (default: public) and save it as a migration")
	f.StringSliceVar(&opts.includeSchemas, "schema", []string{"public"}, "name of Postgres schema to export as a migration. provide multiple schemas with a comma separated list e.g. --schema public,user")
	f.StringVar(&opts.metaDataFile, "metadata-from-file", "", "path to a hasura metadata file to be used for up actions")
	f.BoolVar(&opts.metaDataServer, "metadata-from-server", false, "take metadata from the server and write it as an up migration file")
	f.StringVar(&opts.upSQL, "up-sql", "", "sql string/query that is to be used to create an up migration")
	f.StringVar(&opts.downSQL, "down-sql", "", "sql string/query that is to be used to create a down migration")

	if err := f.MarkDeprecated("sql-from-server", "use --from-server instead"); err != nil {
		ec.Logger.Debugf("marking flag --sql-from-server as depricatef failed: %v", err)
	}
	if err := migrateCreateCmd.MarkFlagFilename("sql-from-file", "sql"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	if err := migrateCreateCmd.MarkFlagFilename("metadata-from-file", "json"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	return migrateCreateCmd
}

type migrateCreateOptions struct {
	EC *cli.ExecutionContext

	name  string
	flags *pflag.FlagSet

	// Flags
	fromServer     bool
	sqlFile        string
	sqlServer      bool
	metaDataFile   string
	metaDataServer bool
	includeSchemas []string
	excludeSchemas []string
	upSQL          string
	upSQLChanged   bool
	downSQLChanged bool
	downSQL        string
	Source         cli.Source
}

func (o *migrateCreateOptions) run() (version int64, err error) {
	var op herrors.Op = "migrate.migrateCreateOptions.run"
	timestamp := getTime()
	createOptions := mig.New(timestamp, o.name, filepath.Join(o.EC.MigrationDir, o.Source.Name))

	if o.fromServer {
		o.sqlServer = true
	}

	var migrateDrv *migrate.Migrate
	// disabling auto state migrations for migrate create command
	o.EC.DisableAutoStateMigration = true
	if o.sqlServer || o.upSQLChanged || o.downSQLChanged {
		migrateDrv, err = migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
		if err != nil {
			return 0, herrors.E(op, fmt.Errorf("cannot create migrate instance: %w", err))
		}
	}

	if o.sqlFile != "" {
		// sql-file flag is set
		err := createOptions.SetSQLUpFromFile(o.sqlFile)
		if err != nil {
			return 0, herrors.E(op, fmt.Errorf("cannot set sql file: %w", err))
		}
	}
	if o.sqlServer {
		data, err := migrateDrv.ExportSchemaDump(o.includeSchemas, o.excludeSchemas, o.Source.Name, o.Source.Kind)
		if err != nil {
			return 0, herrors.E(op, fmt.Errorf("cannot fetch schema dump: %w", err))
		}
		err = createOptions.SetSQLUp(string(data))
		if err != nil {
			return 0, herrors.E(op, fmt.Errorf("while writing data from server into the up.sql file: %w", err))
		}
	}

	// create pure sql based migrations here
	if o.upSQLChanged {
		err = createOptions.SetSQLUp(o.upSQL)
		if err != nil {
			return 0, herrors.E(op, fmt.Errorf("up migration with SQL string could not be created: %w", err))
		}
	}

	if o.downSQLChanged {
		err = createOptions.SetSQLDown(o.downSQL)
		if err != nil {
			return 0, herrors.E(op, fmt.Errorf("down migration with SQL string could not be created: %w", err))
		}
	}

	if o.sqlFile == "" && !o.sqlServer && o.EC.Config.Version != cli.V1 {
		// Set empty data for [up|down].sql
		if !o.upSQLChanged {
			createOptions.SQLUp = []byte(``)
		}
		if !o.downSQLChanged {
			createOptions.SQLDown = []byte(``)
		}
	}

	defer func() {
		if err != nil {
			if err := createOptions.Delete(); err != nil {
				o.EC.Logger.Warnf("cannot delete dangling migrations: %v", err)
			}
		}
	}()
	err = createOptions.Create()
	if err != nil {
		return 0, herrors.E(op, fmt.Errorf("error creating migration files: %w", err))
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Infof("Created Migrations")
	if o.fromServer {
		opts := &MigrateApplyOptions{
			EC:               o.EC,
			SkipExecution:    true,
			VersionMigration: fmt.Sprintf("%d", timestamp),
			Source:           o.Source,
		}
		err := opts.Run()
		if err != nil {
			o.EC.Logger.Warnf("cannot mark created migration %d as applied: %v", timestamp, err)
			o.EC.Logger.Warnf("manually mark it as applied using command:  hasura migrate apply --skip-execution --version %d", timestamp)
		}
	}
	return timestamp, nil
}

func getTime() int64 {
	startTime := time.Now()
	return startTime.UnixNano() / int64(time.Millisecond)
}
