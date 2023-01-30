package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/scripts"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	mig "github.com/hasura/graphql-engine/cli/v2/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	// Initialize migration drivers
	_ "github.com/hasura/graphql-engine/cli/v2/migrate/database/hasuradb"
	_ "github.com/hasura/graphql-engine/cli/v2/migrate/source/file"
)

// NewMigrateCmd returns the migrate command
func NewMigrateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	migrateCmd := &cobra.Command{
		Use:   "migrate",
		Short: "Manage migrations on the database",
		Long: `This command, when used with a collection of subcommands, allows you to manage migrations on the database.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-migrations/
`,
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PersistentPreRunE")
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return errors.E(op, err)
			}
			if err := ec.Validate(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	f := migrateCmd.PersistentFlags()
	f.StringVar(&ec.Source.Name, "database-name", "", "database on which operation should be applied")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	f.Bool("insecure-skip-tls-verify", false, "skip TLS verification and disable cert checking (default: false)")
	f.String("certificate-authority", "", "path to a cert file for the certificate authority")
	f.Bool("disable-interactive", false, "disables interactive prompts (default: false)")

	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))
	util.BindPFlag(v, "disable_interactive", f.Lookup("disable-interactive"))

	f.BoolVar(&ec.DisableAutoStateMigration, "disable-auto-state-migration", false, "after a config v3 update, disable automatically moving state from hdb_catalog.schema_migrations to catalog state")
	if err := f.MarkHidden("disable-auto-state-migration"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	migrateCmd.AddCommand(
		newMigrateApplyCmd(ec),
		newMigrateStatusCmd(ec),
		newMigrateCreateCmd(ec),
		newMigrateSquashCmd(ec),
		newMigrateDeleteCmd(ec),
	)

	return migrateCmd
}

var errDatabaseNotFound = fmt.Errorf("database not found")
var errDatabaseNameNotSet = fmt.Errorf("--database-name flag is required")

// ExecuteMigration runs the actual migration
func ExecuteMigration(cmd string, t *migrate.Migrate, stepOrVersion int64) error {
	var op errors.Op = "commands.ExecuteMigration"
	var err error

	switch cmd {
	case "up":
		err = mig.UpCmd(t, stepOrVersion)
	case "down":
		err = mig.DownCmd(t, stepOrVersion)
	case "gotoVersion":
		err = mig.GotoVersionCmd(t, stepOrVersion)
	case "version":
		var direction string
		if stepOrVersion >= 0 {
			direction = "up"
		} else {
			direction = "down"
			stepOrVersion = -(stepOrVersion)
		}
		err = mig.GotoCmd(t, uint64(stepOrVersion), direction)
	default:
		err = fmt.Errorf("invalid command")
	}

	if err != nil {
		return errors.E(op, err)
	}

	return nil
}

func executeStatus(t *migrate.Migrate) (*migrate.Status, error) {
	var op errors.Op = "commands.executeStatus"
	status, err := t.GetStatus()
	if err != nil {
		return nil, errors.E(op, err)
	}
	return status, nil
}

func validateConfigV3Flags(cmd *cobra.Command, ec *cli.ExecutionContext) error {
	var op errors.Op = "commands.validateConfigV3Flags"
	if err := validateConfigV3Prechecks(cmd, ec); err != nil {
		return errors.E(op, err)
	}
	if ec.Config.Version < cli.V3 {
		return nil
	}

	if err := databaseChooser(ec); err != nil {
		return errors.E(op, err)
	}

	if err := validateSourceInfo(ec); err != nil {
		return errors.E(op, err)
	}

	// check if migration ops are supported for the database
	if !migrate.IsMigrationsSupported(ec.Source.Kind) {
		return errors.E(op, fmt.Errorf("migrations on database '%s' of kind '%s' is not supported", ec.Source.Name, ec.Source.Kind))
	}
	return nil
}

func validateConfigV3FlagsWithAll(cmd *cobra.Command, ec *cli.ExecutionContext) error {
	var op errors.Op = "commands.validateConfigV3FlagsWithAll"
	if err := validateConfigV3Prechecks(cmd, ec); err != nil {
		return errors.E(op, err)
	}
	if ec.Config.Version < cli.V3 {
		return nil
	}
	// if --all-databases flag is present, ignore --database-name and showing UI prompt for choosing a single database
	if cmd.Flags().Changed("all-databases") {
		return nil
	}

	if err := databaseChooserWithAllOption(ec); err != nil {
		return errors.E(op, err)
	}

	if ec.AllDatabases {
		return nil
	}
	if err := validateSourceInfo(ec); err != nil {
		return errors.E(op, err)
	}

	// check if migration ops are supported for the database
	if !migrate.IsMigrationsSupported(ec.Source.Kind) {
		return errors.E(op, fmt.Errorf("migrations on database '%s' of kind '%s' is not supported", ec.Source.Name, ec.Source.Kind))
	}

	return nil
}

func validateConfigV3Prechecks(cmd *cobra.Command, ec *cli.ExecutionContext) error {
	var op errors.Op = "commands.validateConfigV3Prechecks"
	// for project using config older than v3, use PG source kind
	if ec.Config.Version < cli.V3 {
		ec.Source.Kind = hasura.SourceKindPG
		if err := scripts.CheckIfUpdateToConfigV3IsRequired(ec); err != nil {
			return errors.E(op, err)
		}
		return nil
	}

	// for project using config equal to or greater than v3
	// database-name flag is required when running in non-terminal mode
	if (!ec.IsTerminal || ec.Config.DisableInteractive) && !cmd.Flags().Changed("all-databases") && !cmd.Flags().Changed("database-name") {
		return errors.E(op, errDatabaseNameNotSet)
	}

	return nil
}

func validateSourceInfo(ec *cli.ExecutionContext) error {
	var op errors.Op = "commands.validateSourceInfo"
	// find out the database kind by making a API call to server
	// and update ec to include the database name and kind
	sourceKind, err := metadatautil.GetSourceKind(ec.APIClient.V1Metadata.ExportMetadata, ec.Source.Name)
	if err != nil {
		return errors.E(op, fmt.Errorf("determining database kind of '%s': %w", ec.Source.Name, err))
	}
	if sourceKind == nil {
		return errors.E(op, fmt.Errorf("%w: error determining database kind for '%s', check if database exists on hasura", errDatabaseNotFound, ec.Source.Name))
	}
	ec.Source.Kind = *sourceKind
	return nil
}

func databaseChooser(ec *cli.ExecutionContext) error {
	var op errors.Op = "commands.databaseChooser"
	// prompt UI for choosing database if source name is not set
	if ec.Source.Name == "" {
		databaseName, err := metadatautil.DatabaseChooserUI(ec.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			return errors.E(op, err)
		}
		ec.Source.Name = databaseName
	}
	return nil
}

func databaseChooserWithAllOption(ec *cli.ExecutionContext) error {
	var op errors.Op = "commands.databaseChooserWithAllOption"
	// prompt UI for choosing database if source name is not set
	if ec.Source.Name == "" {
		databaseName, err := metadatautil.DatabaseChooserUIWithAll(ec.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			return errors.E(op, err)
		}
		if databaseName == metadatautil.ChooseAllDatabases {
			ec.AllDatabases = true
			return nil
		}
		ec.Source.Name = databaseName
	}
	return nil
}
