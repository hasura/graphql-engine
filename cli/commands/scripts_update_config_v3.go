package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/scripts"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/spf13/afero"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newUpdateMultipleSources(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	var opts scripts.UpdateProjectV3Opts
	cmd := &cobra.Command{
		Use:          "update-project-v3",
		Short:        "Update the Hasura Project from config v2 to v3",
		Long:         "This helper script upgrades your CLI project to use config v3. This process is completely independent from your Hasura Graphql Engine server update process.",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
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
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if opts.Force && len(opts.TargetDatabase) == 0 {
				return errors.E(op, fmt.Errorf("--database-name is required when --force is set"))
			}
			opts.Fs = afero.NewOsFs()
			opts.ProjectDirectory = ec.ExecutionDirectory
			opts.MigrationsAbsDirectoryPath = ec.MigrationDir
			opts.SeedsAbsDirectoryPath = ec.SeedsDirectory
			opts.Logger = ec.Logger
			opts.EC = ec
			if err := scripts.UpdateProjectV3(opts); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	f := cmd.Flags()
	f.StringVar(&opts.TargetDatabase, "database-name", "", "database name for which the current migrations / seeds belong to")
	f.BoolVar(&opts.Force, "force", false, "do not ask for confirmation")
	f.BoolVar(&opts.MoveStateOnly, "move-state-only", false, "do only a state migration from old hdb_catalog.* table to catalog state and skip others")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
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
