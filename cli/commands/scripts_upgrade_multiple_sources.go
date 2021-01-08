package commands

import (
	"github.com/hasura/graphql-engine/cli/internal/scripts"
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
				Logger:                     ec.Logger,
				EC:                         ec,
			}
			return scripts.UpgradeProjectToMultipleSources(opts)
		},
	}
	return cmd
}
