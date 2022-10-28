package commands

import (
	"fmt"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
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
  hasura seed apply --file 1234_add_some_seed_data.sql --database-name default`,
		SilenceUsage: false,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			if err := validateConfigV3Prechecks(cmd, ec); err != nil {
				return err
			}
			if ec.Config.Version < cli.V3 {
				return nil
			}

			if err := databaseChooserWithAllOption(ec); err != nil {
				return err
			}

			if ec.AllDatabases {
				return nil
			}

			if err := validateSourceInfo(ec); err != nil {
				return err
			}
			// check if seed ops are supported for the database
			if !seed.IsSeedsSupported(ec.Source.Kind) {
				return fmt.Errorf("seed operations on database %s of kind %s is not supported", ec.Source.Name, ec.Source.Kind)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.Driver = getSeedDriver(ec.Config.Version)
			if err := opts.Run(); err != nil {
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
	o.EC.Spin("Applying seeds...")
	defer o.EC.Spinner.Stop()
	if o.EC.AllDatabases {
		sourcesAndKind, err := metadatautil.GetSourcesAndKind(o.EC.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			return fmt.Errorf("got error while getting the sources list : %v", err)
		}
		for _, source := range sourcesAndKind {
			o.Source = cli.Source(source)
			err := o.ApplyOnSource()
			if err != nil {
				return fmt.Errorf("error while applying seeds for database %s: %v", o.Source.Name, err)
			}
		}
		return nil
	}
	o.Source = o.EC.Source
	return o.ApplyOnSource()
}

func (o *SeedApplyOptions) ApplyOnSource() error {
	fs := afero.NewOsFs()
	return o.Driver.ApplySeedsToDatabase(fs, o.EC.SeedsDirectory, o.FileNames, o.Source)
}
