package commands

import (
	"io/ioutil"
	"os"
	"time"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata"
	metadataTypes "github.com/hasura/graphql-engine/cli/metadata/types"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"

	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	log "github.com/sirupsen/logrus"
)

const migrateCreateCmdExamples = `  # Setup migration files for the first time by introspecting a server:
  hasura migrate create "init" --from-server

  # Use with admin secret:
  hasura migrate create --admin-secret "<admin-secret>"

  # Setup migration files from an instance mentioned by the flag:
  hasura migrate create init --from-server --endpoint "<endpoint>"`

func newMigrateCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
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
	f.BoolVar(&opts.fromServer, "from-server", false, "take pg_dump of schema and Hasura metadata from the server. use --schema flag to give schemas (default: public)")
	f.StringVar(&opts.sqlFile, "sql-from-file", "", "path to an sql file which contains the SQL statements")
	f.BoolVar(&opts.sqlServer, "sql-from-server", false, "take pg_dump from server and save it as a migration, use --schema flag to give schemas (default: public)")
	f.StringSliceVar(&opts.schemaNames, "schema", []string{"public"}, "name of Postgres schema to export as a migration. provide multiple schemas with a comma separated list e.g. --schema public,user")
	f.StringVar(&opts.metaDataFile, "metadata-from-file", "", "path to a hasura metadata file to be used for up actions")
	f.BoolVar(&opts.metaDataServer, "metadata-from-server", false, "take metadata from the server and write it as an up migration file")

	migrateCreateCmd.MarkFlagFilename("sql-from-file")
	migrateCreateCmd.MarkFlagFilename("metadata-from-file")

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
	schemaNames    []string
}

func (o *migrateCreateOptions) run() (version int64, err error) {
	timestamp := getTime()
	createOptions := mig.New(timestamp, o.name, o.EC.MigrationDir)

	if o.fromServer {
		o.sqlServer = true
		if o.EC.Config.Version == cli.V1 {
			o.metaDataServer = true
		}
	}

	if o.flags.Changed("metadata-from-file") && o.EC.Config.Version != cli.V1 {
		return 0, errors.New("metadata-from-file flag can be set only with config version 1")
	}

	if o.flags.Changed("metadata-from-server") && o.EC.Config.Version != cli.V1 {
		return 0, errors.New("metadata-from-server flag can be set only with config version 1")
	}

	if o.flags.Changed("metadata-from-file") && o.sqlServer {
		return 0, errors.New("only one sql type can be set")
	}
	if o.flags.Changed("metadata-from-file") && o.metaDataServer {
		return 0, errors.New("only one metadata type can be set")
	}

	var migrateDrv *migrate.Migrate
	if o.sqlServer || o.metaDataServer {
		migrateDrv, err = migrate.NewMigrate(o.EC, true)
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

	if o.flags.Changed("metadata-from-file") {
		// metadata-file flag is set
		err := createOptions.SetMetaUpFromFile(o.metaDataFile)
		if err != nil {
			return 0, errors.Wrap(err, "cannot set metadata file")
		}
	}

	// Create metadata migrations only if config version is V1
	if o.metaDataServer {
		// To create metadata.yaml, set metadata plugin
		tmpDirName, err := ioutil.TempDir("", "*")
		if err != nil {
			return 0, errors.Wrap(err, "cannot create temp directory to fetch metadata")
		}
		defer os.RemoveAll(tmpDirName)
		plugins := make(metadataTypes.MetadataPlugins, 0)
		plugins = append(plugins, metadata.New(o.EC, tmpDirName))
		migrateDrv.SetMetadataPlugins(plugins)
		// fetch metadata from server
		files, err := migrateDrv.ExportMetadata()
		if err != nil {
			return 0, errors.Wrap(err, "cannot fetch metadata from server")
		}
		err = migrateDrv.WriteMetadata(files)
		if err != nil {
			return 0, errors.Wrap(err, "cannot write to tmp file")
		}

		for name := range files {
			err = createOptions.SetMetaUpFromFile(name)
			if err != nil {
				return 0, errors.Wrap(err, "cannot parse metadata from the server")
			}
		}
	}

	if !o.flags.Changed("sql-from-file") && !o.flags.Changed("metadata-from-file") && !o.metaDataServer && !o.sqlServer && o.EC.Config.Version == cli.V1 {
		// Set empty data for [up|down].yaml
		createOptions.MetaUp = []byte(`[]`)
		createOptions.MetaDown = []byte(`[]`)
	}

	if !o.flags.Changed("sql-from-file") && !o.flags.Changed("metadata-from-file") && !o.metaDataServer && !o.sqlServer && o.EC.Config.Version != cli.V1 {
		// Set empty data for [up|down].sql
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
