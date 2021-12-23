package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/sirupsen/logrus"
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
			logger := logrus.New()
			logger.SetOutput(ec.Stdout)
			logger.SetFormatter(&logrus.TextFormatter{DisableTimestamp: true, DisableColors: ec.NoColor})
			if !ec.IsTerminal {
				logger.SetFormatter(&logrus.JSONFormatter{PrettyPrint: false})
			}

			logger.WithField("version", ec.Version.GetCLIVersion()).Info("hasura cli")
			err := ec.Validate()
			if err == nil {
				logger.
					WithField("endpoint", ec.Config.ServerConfig.Endpoint).
					WithField("version", ec.Version.GetServerVersion()).
					Info("hasura graphql engine")
			}
			return nil
		},
	}
	return versionCmd
}
