package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

// newMigrateSettingsCmd returns the version command
func newMigrateSettingsCmd(ec *cli.ExecutionContext) *cobra.Command {
	migrateSettingsCmd := &cobra.Command{
		Use:          "settings",
		Short:        "Manage migration settings",
		SilenceUsage: true,
	}
	migrateSettingsCmd.AddCommand(
		newMigrateSettingsGetCmd(ec),
		newMigrateSettingsSetCmd(ec),
	)
	return migrateSettingsCmd
}
