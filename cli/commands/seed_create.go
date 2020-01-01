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

const migrateSeedCmdExamples = `  # Setup seed files for tables:
  hasura migrate seed --endpoint "<endpoint>" 

  # Create seeds for specified tables only:
  hasura migrate create --tables one,two,three

  # Use with admin secret:
  hasura migrate seed --admin-secret "<admin-secret>"`

func newSeedCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateSeedOptions{
		EC: ec,
	}

	migrateSeedCmd := &cobra.Command{
		Use:          "create [migration-name]",
		Short:        "Create files required for seeding",
		Long:         "Create sql files required for seeding",
		Example:      migrateSeedCmdExamples,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			opts.EC.Spin("Creating seed files...")
			version, err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
			}
			opts.EC.Logger.WithFields(log.Fields{
				"version": version,
				"name":    opts.name,
			}).Info("Seed files created")
			return nil
		},
	}
	f := migrateSeedCmd.Flags()
	opts.flags = f
	f.StringArrayVar(&opts.schemaNames, "schema", []string{"public"}, "name of Postgres schema to export seed data from")
	f.StringSliceVar(&opts.tableNames, "tables", []string{}, "name of schema tables to create seed data for (optional)")
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return migrateSeedCmd
}

type migrateSeedOptions struct {
	EC *cli.ExecutionContext

	name  string
	flags *pflag.FlagSet

	// Flags
	schemaNames []string
	tableNames  []string
}

func (o *migrateSeedOptions) run() (version int64, err error) {
	timestamp := getTimeSeed()
	createOptions := mig.New(timestamp, o.name, o.EC.SeedDir)

	var migrateDrv *migrate.Migrate
	migrateDrv, err = newMigrate(o.EC.SeedDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return 0, errors.Wrap(err, "cannot create seed instance")
	}

	data, err := migrateDrv.ExportDataDump(o.schemaNames, o.tableNames)
	if err != nil {
		return 0, errors.Wrap(err, "cannot fetch schema dump")
	}
	createOptions.SetSQLUp(string(data))

	defer func() {
		if err != nil {
			createOptions.Delete()
		}
	}()
	err = createOptions.Create()
	if err != nil {
		return 0, errors.Wrap(err, "error creating seed files")
	}
	return timestamp, nil
}

func getTimeSeed() int64 {
	startTime := time.Now()
	return startTime.UnixNano() / int64(time.Millisecond)
}
