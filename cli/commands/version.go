package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewVersionCmd returns the version command
func NewVersionCmd(ec *cli.ExecutionContext) *cobra.Command {
	versionCmd := &cobra.Command{
		Use:          "version",
		Short:        "Print the CLI version",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = viper.New()
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			ec.Logger.WithField("version", ec.Version.GetCLIVersion()).Info("hasura cli")
			err := ec.Validate()
			if err == nil {
				ec.Logger.
					WithField("endpoint", ec.Config.ServerConfig.Endpoint).
					WithField("version", ec.Version.GetServerVersion()).
					Info("hasura graphql engine")
			}
			return nil
		},
	}
	return versionCmd
}
