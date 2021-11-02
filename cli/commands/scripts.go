package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
)

// NewScriptsCmd returns the scripts command
func NewScriptsCmd(ec *cli.ExecutionContext) *cobra.Command {
	scriptsCmd := &cobra.Command{
		Use:          "scripts",
		Short:        "Execute helper scripts to manage Hasura projects",
		SilenceUsage: true,
	}
	scriptsCmd.AddCommand(
		newScriptsUpdateConfigV2Cmd(ec),
		newUpdateMultipleSources(ec),
	)
	return scriptsCmd
}
