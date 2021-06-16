package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2/seed"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/internal/scripts"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewSeedCmd will return the seed command
func NewSeedCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	ec.Viper = v
	seedCmd := &cobra.Command{
		Use:          "seed",
		Aliases:      []string{"sd", "seeds"},
		Short:        "Manage seed data",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			err := ec.Prepare()
			if err != nil {
				return err
			}
			if err := ec.Validate(); err != nil {
				return err
			}
			// for project using config older than v3, use PG source kind
			if ec.Config.Version < cli.V3 {
				ec.Source.Kind = hasura.SourceKindPG
				if err := scripts.CheckIfUpdateToConfigV3IsRequired(ec); err != nil {
					return err
				}
				return nil
			}

			// for project using config equal to or greater than v3
			// database-name flag is required when running in non-terminal mode
			if (!ec.IsTerminal || ec.Config.DisableInteractive) && !cmd.Flags().Changed("database-name") {
				return errors.New("--database-name flag is required")
			}

			// prompt UI for choosing database if source name is not set
			if ec.Source.Name == "" {
				databaseName, err := metadatautil.DatabaseChooserUI(ec.APIClient.V1Metadata.ExportMetadata)
				if err != nil {
					return err
				}
				ec.Source.Name = databaseName
			}

			sourceKind, err := metadatautil.GetSourceKind(ec.APIClient.V1Metadata.ExportMetadata, ec.Source.Name)
			if err != nil {
				return err
			}
			if sourceKind == nil {
				return fmt.Errorf("cannot determine source kind for %v", ec.Source.Name)
			}
			ec.Source.Kind = *sourceKind

			// check if seed ops are supported for the database
			if !seed.IsSeedsSupported(*sourceKind) {
				return fmt.Errorf("seed operations on database %s of kind %s is not supported", ec.Source.Name, *sourceKind)
			}

			return nil
		},
	}

	seedCmd.AddCommand(
		newSeedCreateCmd(ec),
		newSeedApplyCmd(ec),
	)

	f := seedCmd.PersistentFlags()
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

	return seedCmd
}

func getSeedDriver(configVersion cli.ConfigVersion) (driver *seed.Driver) {
	if configVersion >= cli.V3 {
		driver = seed.NewDriver(ec.APIClient.V2Query.Bulk, ec.APIClient.PGDump)
	} else {
		driver = seed.NewDriver(ec.APIClient.V1Query.Bulk, ec.APIClient.PGDump)
	}
	return driver
}
