package commands

import (
	"net/url"
	"strconv"

	"github.com/hasura/graphql-engine/cli"
	migrate "github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMigrateApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &migrateApplyOptions{
		EC: ec,
	}
	migrateApplyCmd := &cobra.Command{
		Use:          "apply",
		Short:        "Apply migrations on the database",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}
	f := migrateApplyCmd.Flags()

	f.StringVar(&opts.upMigration, "up", "", "apply all or N up migration steps")
	f.StringVar(&opts.downMigration, "down", "", "apply all or N down migration steps")
	f.StringVar(&opts.versionMigration, "version", "", "migrate the database to a specific version")
	f.StringVar(&opts.migrationType, "type", "up", "type of migration (up, down) to be used with version flag")
	return migrateApplyCmd
}

type migrateApplyOptions struct {
	EC *cli.ExecutionContext

	upMigration      string
	downMigration    string
	versionMigration string
	migrationType    string
}

func (o *migrateApplyOptions) run() error {
	migrationType, step, err := getMigrationTypeAndStep(o.upMigration, o.downMigration, o.versionMigration, o.migrationType)
	if err != nil {
		return errors.Wrap(err, "error validating flags")
	}

	dbURL, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "error parsing endpoint")
	}

	dbURL.Scheme = "hasuradb"
	dbURL.User = url.UserPassword("admin", o.EC.Config.AccessKey)

	err = util.ExecuteMigration(migrationType, "file://"+o.EC.MigrationDir, dbURL.String(), step)
	if err != nil {
		if err == migrate.ErrNoChange {
			o.EC.Logger.Info("nothing to apply")
			return nil
		}
		return errors.Wrap(err, "apply failed")
	}
	o.EC.Logger.Info("migrations applied")
	return nil
}

// Only one flag out of up, down and version can be set at a time. This function
// checks whether that is the case and returns an error is not
func getMigrationTypeAndStep(upMigration, downMigration, versionMigration, migrationType string) (string, int64, error) {
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

	if stepString == "all" && migrationName != "version" {
		return migrationName, -1, nil
	}

	step, err := strconv.ParseInt(stepString, 10, 64)
	if err != nil {
		return "", 0, errors.Wrap(err, "not a valid input for steps")
	}
	return migrationName, step, nil
}
