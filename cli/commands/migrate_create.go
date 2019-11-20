package commands

import (
	"time"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
	"github.com/spf13/viper"

	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	log "github.com/sirupsen/logrus"
)

const migrateCreateCmdExamples = `  # Setup migration files for the first time by introspecting a server:
  hasura migrate create "init" --sql-from-server`

func newMigrateCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateCreateOptions{
		EC: ec,
	}

	migrateCreateCmd := &cobra.Command{
		Use:          "create [migration-name]",
		Short:        "Create files required for a migration",
		Long:         "Create sql and yaml files required for a migration",
		Example:      migrateCreateCmdExamples,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			opts.EC.Spin("Creating migration files...")
			version, err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
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
	f.StringVar(&opts.sqlFile, "sql-from-file", "", "path to an sql file which contains the SQL statements")
	f.BoolVar(&opts.sqlServer, "sql-from-server", false, "take pg_dump from server and save it as a migration")
	f.StringArrayVar(&opts.schemaNames, "schema", []string{"public"}, "name of Postgres schema to export as migration")
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	migrateCreateCmd.MarkFlagFilename("sql-from-file")
	migrateCreateCmd.MarkFlagFilename("metadata-from-file")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return migrateCreateCmd
}

type migrateCreateOptions struct {
	EC *cli.ExecutionContext

	name  string
	flags *pflag.FlagSet

	// Flags
	sqlFile     string
	sqlServer   bool
	schemaNames []string
}

func (o *migrateCreateOptions) run() (version int64, err error) {
	timestamp := getTime()
	createOptions := mig.New(timestamp, o.name, o.EC.MigrationDir)

	if o.flags.Changed("sql-from-file") && o.sqlServer {
		return 0, errors.New("only one sql type can be set")
	}

	var migrateDrv *migrate.Migrate
	if o.sqlServer {
		migrateDrv, err = newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
		if err != nil {
			return 0, errors.Wrap(err, "cannot create migrate instance")
		}
	}

	if o.flags.Changed("sql-from-file") {
		// sql-file flag is set
		err := createOptions.SetSQLUpFromFile(o.sqlFile)
		if err != nil {
			return 0, errors.Wrap(err, "cannot set sql file")
		}
	}
	if o.sqlServer {
		data, err := migrateDrv.ExportSchemaDump(o.schemaNames)
		if err != nil {
			return 0, errors.Wrap(err, "cannot fetch schema dump")
		}
		createOptions.SetSQLUp(string(data))
	}

	if !o.flags.Changed("sql-from-file") && !o.sqlServer {
		// Set empty data for [up|down].yaml
		createOptions.SQLUp = []byte(``)
		createOptions.SQLDown = []byte(``)
	}

	defer func() {
		if err != nil {
			createOptions.Delete()
		}
	}()
	err = createOptions.Create()
	if err != nil {
		return 0, errors.Wrap(err, "error creating migration files")
	}
	return timestamp, nil
}

func getTime() int64 {
	startTime := time.Now()
	return startTime.UnixNano() / int64(time.Millisecond)
}
