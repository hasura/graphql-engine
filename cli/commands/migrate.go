package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/internal/metadatautil"

	"github.com/hasura/graphql-engine/cli/internal/hasura"
	"github.com/hasura/graphql-engine/cli/internal/scripts"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	// Initialize migration drivers
	_ "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	_ "github.com/hasura/graphql-engine/cli/migrate/source/file"
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

	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))

	migrateCmd.AddCommand(
		newMigrateApplyCmd(ec),
		newMigrateStatusCmd(ec),
		newMigrateCreateCmd(ec),
		newMigrateSquashCmd(ec),
	)

	return migrateCmd
}

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
	if ec.Config.Version >= cli.V3 {
		if !cmd.Flags().Changed("database-name") {
			return errDatabaseNameNotSet{"--database-name flag is required"}
		} else {
			// if database-name flag is set
			// find out the database kind by making a API call to server
			// and update ec to include the database name and kind
			sourceKind, err := metadatautil.GetSourceKind(ec.APIClient.V1Metadata.ExportMetadata, ec.Source.Name)
			if err != nil {
				return fmt.Errorf("determining database kind of %s: %w", ec.Source.Name, err)
			}
			if sourceKind == nil {
				return fmt.Errorf("error determining database kind for %s, check if database exists on hasura", ec.Source.Name)
			}
			ec.Source.Kind = *sourceKind

			if !migrate.IsMigrationsSupported(*sourceKind) {
				return fmt.Errorf("migrations on source %s of kind %s is not supported", ec.Source.Name, *sourceKind)
			}
		}
	} else {
		// for project using config older than v3, use PG source kind
		ec.Source.Kind = hasura.SourceKindPG
		if err := scripts.CheckIfUpdateToConfigV3IsRequired(ec); err != nil {
			return err
		}
	}
	return nil
}

type errDatabaseNameNotSet struct {
	message string
}

func (e errDatabaseNameNotSet) Error() string {
	return e.message
}
