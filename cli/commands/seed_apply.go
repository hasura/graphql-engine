package commands

import (
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/seed"
)

type seedApplyOptions struct {
	ec *cli.ExecutionContext

	// seed file to apply
	fileName string
}

func newSeedApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := seedApplyOptions{
		ec: ec,
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
			opts.ec.Spin("Applying seeds...")
			err := opts.run()
			opts.ec.Spinner.Stop()
			if err != nil {
				return err
			}
			opts.ec.Logger.Info("Seeds planted")
			return nil
		},
	}
	cmd.Flags().StringVarP(&opts.fileName, "file", "f", "", "seed file to apply")
	return cmd
}

func (o *seedApplyOptions) run() error {
	migrateDriver, err := migrate.NewMigrate(ec, true)
	if err != nil {
		return err
	}
	fs := afero.NewOsFs()
	return seed.ApplySeedsToDatabase(ec, fs, migrateDriver, o.fileName)
}
