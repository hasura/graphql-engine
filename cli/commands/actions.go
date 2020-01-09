package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewActionsCmd(ec *cli.ExecutionContext) *cobra.Command {
	actionsCmd := &cobra.Command{
		Use:          "actions",
		Short:        "",
		SilenceUsage: true,
	}
	actionsCmd.AddCommand(
		newActionsCreateCmd(ec),
	)
	return actionsCmd
}
