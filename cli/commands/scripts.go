package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewScriptsCmd(ec *cli.ExecutionContext) *cobra.Command {
	scriptsCmd := &cobra.Command{
		Use:          "scripts",
		Short:        "",
		SilenceUsage: true,
	}
	scriptsCmd.AddCommand(
		newScriptsUpdateConfigV2Cmd(ec),
	)
	return scriptsCmd
}
