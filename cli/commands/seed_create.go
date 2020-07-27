package commands

import (
	"bytes"

	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions/editor"
	"github.com/hasura/graphql-engine/cli/seed"
)

type SeedNewOptions struct {
	EC *cli.ExecutionContext

	// filename for the new seed file
	SeedName string
	// table name if seed file has to be created from a database table
	FromTableNames []string

	// seed file that was created
	FilePath string
}

func newSeedCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := SeedNewOptions{
		EC: ec,
	}
	cmd := &cobra.Command{
		Use:   "create seed_name",
		Short: "create a new seed file",
		Example: `  # Create a new seed file and use editor to add SQL:
  hasura seed create new_table_seed

  # Create a new seed by exporting data from tables already present in the database:
  hasura seed create table1_seed --from-table table1

  # Export data from multiple tables:
  hasura seed create tables_seed --from-table table1 --from-table table2`,
		Args:         cobra.ExactArgs(1),
		SilenceUsage: false,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.SeedName = args[0]
			err := opts.Run()
			if err != nil {
				return err
			}
			ec.Logger.WithField("file", opts.FilePath).Info("created seed file successfully")
			return nil
		},
	}

	cmd.Flags().StringArrayVar(&opts.FromTableNames, "from-table", []string{}, "name of table from which seed file has to be initialized")

	return cmd
}

func (o *SeedNewOptions) Run() error {
	createSeedOpts := seed.CreateSeedOpts{
		UserProvidedSeedName: o.SeedName,
		DirectoryPath:        o.EC.SeedsDirectory,
	}

	// If we are initializing from a database table
	// create a hasura client and add table name opts
	if createSeedOpts.Data == nil {
		if len(o.FromTableNames) > 0 {
			migrateDriver, err := migrate.NewMigrate(ec, true)
			if err != nil {
				return errors.Wrap(err, "cannot initialize migrate driver")
			}
			// Send the query
			body, err := migrateDriver.ExportDataDump(o.FromTableNames)
			if err != nil {
				return errors.Wrap(err, "exporting seed data")
			}

			createSeedOpts.Data = bytes.NewReader(body)
		} else {
			const defaultText = ""
			data, err := editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, defaultText, "*.sql")
			if err != nil {
				return errors.Wrap(err, "cannot find default editor from env")
			}
			createSeedOpts.Data = bytes.NewReader(data)
		}
	}

	fs := afero.NewOsFs()
	filepath, err := seed.CreateSeedFile(fs, createSeedOpts)
	if err != nil || filepath == nil {
		return errors.Wrap(err, "failed to create seed file")
	}

	o.FilePath = *filepath

	return nil
}
