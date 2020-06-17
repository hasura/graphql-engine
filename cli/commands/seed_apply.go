package commands

import (
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/seed"
)

type SeedApplyOptions struct {
	EC *cli.ExecutionContext

	// seed file to apply
	FileNames []string
}

func newSeedApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := SeedApplyOptions{
		EC: ec,
	}
	cmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply seed data",
		Example: `  # Apply all seeds on the database:
  hasura seed apply

  # Apply only a particular file:
  hasura seed apply --file seeds/1234_add_some_seed_data.sql`,
		SilenceUsage: false,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Applying seeds...")
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return err
			}
			opts.EC.Logger.Info("Seeds planted")
			return nil
		},
	}
	cmd.Flags().StringArrayVarP(&opts.FileNames, "file", "f", []string{}, "seed file to apply")
	return cmd
}

func (o *SeedApplyOptions) Run() error {
	migrateDriver, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return err
	}
	fs := afero.NewOsFs()
	return seed.ApplySeedsToDatabase(o.EC, fs, migrateDriver, o.FileNames)
}
