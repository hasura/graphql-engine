package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewMigrateCmd(ec *cli.ExecutionContext) *cobra.Command {
	migrateCmd := &cobra.Command{
		Use:          "migrate",
		Short:        "Manage migrations on the database",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Validate()
		},
	}
	migrateCmd.AddCommand(
		NewMigrateApplyCmd(ec),
		NewMigrateStatusCmd(ec),
		NewMigrateCreateCmd(ec),
	)
	return migrateCmd
}
