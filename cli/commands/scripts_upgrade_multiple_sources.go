package commands

import (
	"github.com/hasura/graphql-engine/cli/internal/scripts"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/spf13/afero"

	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newUpdateMultipleSources(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	cmd := &cobra.Command{
		Use:   "update-project-v3",
		Short: "update project to use config v2 to config v3",
		Long: `
`,
		Example:      `  `,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts := scripts.UpgradeToMuUpgradeProjectToMultipleSourcesOpts{
				Fs:                         afero.NewOsFs(),
				ProjectDirectory:           ec.ExecutionDirectory,
				MigrationsAbsDirectoryPath: ec.MigrationDir,
				SeedsAbsDirectoryPath:      ec.SeedsDirectory,
				Logger:                     ec.Logger,
				EC:                         ec,
			}
			return scripts.UpdateProjectV3(opts)
		},
	}

	f := cmd.Flags()

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")
	f.Bool("insecure-skip-tls-verify", false, "skip TLS verification and disable cert checking (default: false)")
	f.String("certificate-authority", "", "path to a cert file for the certificate authority")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))
	return cmd
}
