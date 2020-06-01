package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewSeedCmd will return the seed command
func NewSeedCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	ec.Viper = v
	seedCmd := &cobra.Command{
		Use:     "seeds",
		Aliases: []string{"sd"},
		Short:   "Manage seed data",
		Long: `Seed command let's you create and manage data seed files which can be used
to initialize the database with some data. The files can contain SQL statements
which will be executed when the seed is applied. Seeds will be applied in
alphabetical order of the filenames.`,
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
	}

	seedCmd.AddCommand(
		newSeedCreateCmd(ec),
		newSeedApplyCmd(ec),
	)
	seedCmd.PersistentFlags().String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	seedCmd.PersistentFlags().String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	seedCmd.PersistentFlags().String("access-key", "", "access key for Hasura GraphQL Engine")
	seedCmd.PersistentFlags().MarkDeprecated("access-key", "use --admin-secret instead")

	v.BindPFlag("endpoint", seedCmd.PersistentFlags().Lookup("endpoint"))
	v.BindPFlag("admin_secret", seedCmd.PersistentFlags().Lookup("admin-secret"))
	v.BindPFlag("access_key", seedCmd.PersistentFlags().Lookup("access-key"))

	return seedCmd
}
