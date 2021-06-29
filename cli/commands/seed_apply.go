package commands

import (
	"fmt"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/seed"
)

type SeedApplyOptions struct {
	EC     *cli.ExecutionContext
	Driver *seed.Driver

	// seed file to apply
	FileNames []string
	Source    cli.Source
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
			opts.Driver = getSeedDriver(ec.Config.Version)
			opts.EC.Spin("Applying seeds...")
			opts.Source = ec.Source
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return fmt.Errorf("operation failed \n%w", err)
			}
			opts.EC.Logger.Info("Seeds planted")
			return nil
		},
	}
	cmd.Flags().StringArrayVarP(&opts.FileNames, "file", "f", []string{}, "seed file to apply")
	return cmd
}

func (o *SeedApplyOptions) Run() error {
	fs := afero.NewOsFs()
	return o.Driver.ApplySeedsToDatabase(fs, o.EC.SeedsDirectory, o.FileNames, o.EC.Source)
}
