package commands

import (
	"fmt"
	"github.com/pkg/errors"

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
		Use:          "migrate",
		Short:        "Manage migrations on the database",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			if err := ec.Validate(); err != nil {
				return err
			}
			return nil
		},
	}

	f := migrateCmd.PersistentFlags()
	f.StringVar(&ec.Source.Name, "database-name", "", "database on which operation should be applied")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL engine")
	f.String("access-key", "", "access key for Hasura GraphQL engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")
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
	f.MarkHidden("disable-auto-state-migration")

	migrateCmd.AddCommand(
		newMigrateApplyCmd(ec),
		newMigrateStatusCmd(ec),
		newMigrateCreateCmd(ec),
		newMigrateSquashCmd(ec),
		newMigrateDeleteCmd(ec),
	)

	return migrateCmd
}

var errDatabaseNotFound = errors.New("database not found")
var errDatabaseNameNotSet = errors.New("--database-name flag is required")

// ExecuteMigration runs the actual migration
func ExecuteMigration(cmd string, t *migrate.Migrate, stepOrVersion int64) error {
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

	return err
}

func executeStatus(t *migrate.Migrate) (*migrate.Status, error) {
	status, err := t.GetStatus()
	if err != nil {
		return nil, err
	}
	return status, nil
}

func validateConfigV3Flags(cmd *cobra.Command, ec *cli.ExecutionContext) error {
	// for project using config older than v3, use PG source kind
	if ec.Config.Version < cli.V3 {
		ec.Source.Kind = hasura.SourceKindPG
		if err := scripts.CheckIfUpdateToConfigV3IsRequired(ec); err != nil {
			return err
		}
		return nil
	}

	// if --all-databases flag is present, ignore --database-name and showing UI prompt for choosing a single database
	if cmd.Flags().Changed("all-databases") {
		return nil
	}

	// for project using config equal to or greater than v3
	// database-name flag is required when running in non-terminal mode
	if (!ec.IsTerminal || ec.Config.DisableInteractive) && !cmd.Flags().Changed("database-name") {
		return errDatabaseNameNotSet
	}

	// prompt UI for choosing database if source name is not set
	if ec.Source.Name == "" {
		databaseName, err := metadatautil.DatabaseChooserUI(ec.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			return err
		}
		ec.Source.Name = databaseName
	}

	// find out the database kind by making a API call to server
	// and update ec to include the database name and kind
	sourceKind, err := metadatautil.GetSourceKind(ec.APIClient.V1Metadata.ExportMetadata, ec.Source.Name)
	if err != nil {
		return fmt.Errorf("determining database kind of %s: %w", ec.Source.Name, err)
	}
	if sourceKind == nil {
		return fmt.Errorf("%w: error determining database kind for %s, check if database exists on hasura", errDatabaseNotFound, ec.Source.Name)
	}
	ec.Source.Kind = *sourceKind

	// check if migration ops are supported for the database
	if !migrate.IsMigrationsSupported(*sourceKind) {
		return fmt.Errorf("migrations on source %s of kind %s is not supported", ec.Source.Name, *sourceKind)
	}
	return nil
}
