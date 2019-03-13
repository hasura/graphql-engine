package commands

import (
	"io/ioutil"
	"net/url"
	"os"
	"time"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
	"github.com/spf13/viper"
)

func newMigrateCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateCreateOptions{
		EC: ec,
	}

	migrateCreateCmd := &cobra.Command{
		Use:          "create [migration-name]",
		Short:        "Create files required for a migration",
		Long:         "Create sql and yaml files required for a migration",
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			return opts.run()
		},
	}
	f := migrateCreateCmd.Flags()
	opts.flags = f
	f.StringVar(&opts.sqlFile, "sql-file", "", "")
	f.StringVar(&opts.metaDataFile, "metadata-file", "", "")
	f.StringVar(&opts.metaDataServer, "metadata-from-server", "", "")
	f.StringVar(&opts.adminSecret, "admin-secret", "", "")

	return migrateCreateCmd
}

type migrateCreateOptions struct {
	EC *cli.ExecutionContext

	name  string
	flags *pflag.FlagSet

	// Flags
	sqlFile        string
	metaDataFile   string
	metaDataServer string
	adminSecret    string
}

func (o *migrateCreateOptions) run() error {
	timestamp := getTime()
	createOptions := mig.New(timestamp, o.name, o.EC.MigrationDir)
	createOptions.IsCMD = true

	if o.flags.Changed("sql-file") {
		// sql-file flag is set
		err := createOptions.SetSQLUpFromFile(o.sqlFile)
		if err != nil {
			return errors.Wrap(err, "cannot set sql file")
		}
	}

	if o.flags.Changed("metadata-file") && o.flags.Changed("metadata-from-server") {
		return errors.New("only one metadata type can be set")
	}

	if o.flags.Changed("metadata-file") {
		// metadata-file flag is set
		err := createOptions.SetMetaUpFromFile(o.metaDataFile)
		if err != nil {
			return errors.Wrap(err, "cannot set metadata file")
		}
	}

	if o.flags.Changed("metadata-from-server") {
		// parse metadata-from-server url
		nurl, err := url.Parse(o.metaDataServer)
		if err != nil {
			return errors.Wrap(err, "cannot parse metdata-from-server endpoint")
		}

		// create new migrate instance
		migrateDrv, err := newMigrate(o.EC.MigrationDir, nurl, o.adminSecret, o.EC.Logger, o.EC.Version)
		if err != nil {
			return err
		}

		// fetch metadata from server
		metaData, err := migrateDrv.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot fetch metadata from server")
		}

		tmpfile, err := ioutil.TempFile("", "metadata")
		if err != nil {
			return err
		}
		defer os.Remove(tmpfile.Name())

		t, err := yaml.Marshal(metaData)
		if err != nil {
			return errors.Wrap(err, "Cannot Marshal metadata")
		}
		if _, err := tmpfile.Write(t); err != nil {
			return err
		}
		if err := tmpfile.Close(); err != nil {
			return err
		}

		err = createOptions.SetMetaUpFromFile(tmpfile.Name())
		if err != nil {
			return errors.Wrap(err, "cannot parse metadata from the server")
		}
	}

	err := createOptions.Create()
	if err != nil {
		return errors.Wrap(err, "error creating migration files")
	}

	o.EC.Logger.Infof("Migration files created with version %d_%s.[up|down].[yaml|sql]", timestamp, o.name)
	return nil
}

func getTime() int64 {
	startTime := time.Now()
	return startTime.UnixNano() / int64(time.Millisecond)
}
