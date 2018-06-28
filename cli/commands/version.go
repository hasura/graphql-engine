package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewVersionCmd(ec *cli.ExecutionContext) *cobra.Command {
	versionCmd := &cobra.Command{
		Use:          "version",
		Short:        "Print the CLI version",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			ec.Logger.Infof("hasura cli version: %s", ec.GetVersion())
			return nil
		},
	}
	return versionCmd
}
