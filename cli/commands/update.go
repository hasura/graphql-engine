package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

// NewUpdateCmd checks and update to lastest graphql-engine cli
func NewUpdateCmd(ec *cli.ExecutionContext) *cobra.Command {
	updateCmd := &cobra.Command{
		Use:          "update-cli",
		Short:        "Update graphql-engine CLI tool to latest version",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			cliVersion := ec.Version.GetCLIVersion()
		},
	}
}
