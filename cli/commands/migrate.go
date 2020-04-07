package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	// Initialize migration drivers
	_ "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	_ "github.com/hasura/graphql-engine/cli/migrate/source/file"
)

// NewMigrateCmd returns the migrate command
func NewMigrateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	migrateCmd := &cobra.Command{
		Use:          "migrate",
		Short:        "Manage migrations on the database",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
	}
	migrateCmd.AddCommand(
		newMigrateApplyCmd(ec),
		newMigrateStatusCmd(ec),
		newMigrateCreateCmd(ec),
		newMigrateSquashCmd(ec),
	)
	migrateCmd.PersistentFlags().String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	migrateCmd.PersistentFlags().String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	migrateCmd.PersistentFlags().String("access-key", "", "access key for Hasura GraphQL Engine")
	migrateCmd.PersistentFlags().MarkDeprecated("access-key", "use --admin-secret instead")

	v.BindPFlag("endpoint", migrateCmd.PersistentFlags().Lookup("endpoint"))
	v.BindPFlag("admin_secret", migrateCmd.PersistentFlags().Lookup("admin-secret"))
	v.BindPFlag("access_key", migrateCmd.PersistentFlags().Lookup("access-key"))
	return migrateCmd
}

// ExecuteMigration runs the actual migration
func ExecuteMigration(cmd string, t *migrate.Migrate, stepOrVersion int64) error {
	var err error

	switch cmd {
	case "up":
		err = mig.UpCmd(t, stepOrVersion)
	case "down":
		err = mig.DownCmd(t, stepOrVersion)
	case "gotoVersion":
		err = mig.GotoVersionCmd(t, stepOrVersion)
	case "version":
		var direction string
		if stepOrVersion >= 0 {
			direction = "up"
		} else {
			direction = "down"
			stepOrVersion = -(stepOrVersion)
		}
		err = mig.GotoCmd(t, uint64(stepOrVersion), direction)
	default:
		err = fmt.Errorf("Invalid command")
	}

	return err
}

func executeStatus(t *migrate.Migrate) (*migrate.Status, error) {
	status, err := t.GetStatus()
	if err != nil {
		return nil, err
	}
	return status, nil
}
