package commands

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/pkg/errors"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/editor"
	"github.com/hasura/graphql-engine/cli/v2/seed"
)

type SeedNewOptions struct {
	EC     *cli.ExecutionContext
	Driver *seed.Driver

	// filename for the new seed file
	SeedName string
	// table name if seed file has to be created from a database table
	FromTableNames []string

	// seed file that was created
	FilePath string
	Source   cli.Source
}

func newSeedCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := SeedNewOptions{
		EC: ec,
	}
	cmd := &cobra.Command{
		Use:   "create seed_name",
		Short: "Create a new seed file",
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
			opts.Source = ec.Source
			opts.Driver = getSeedDriver(ec.Config.Version)
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
	databaseDirectory := filepath.Join(o.EC.SeedsDirectory, o.Source.Name)
	if f, _ := os.Stat(databaseDirectory); f == nil {
		if err := os.MkdirAll(databaseDirectory, 0755); err != nil {
			return err
		}
	}
	createSeedOpts := seed.CreateSeedOpts{
		UserProvidedSeedName: o.SeedName,
		DirectoryPath:        filepath.Join(o.EC.SeedsDirectory, o.Source.Name),
	}
	// If we are initializing from a database table
	// create a hasura client and add table name opts
	if createSeedOpts.Data == nil {
		var body []byte
		if len(o.FromTableNames) > 0 {
			if o.Source.Kind != hasura.SourceKindPG && o.EC.Config.Version >= cli.V3 {
				return fmt.Errorf("--from-table is supported only for postgres databases")
			}
			// Send the query
			bodyReader, err := o.Driver.ExportDatadump(o.FromTableNames, o.Source.Name)
			if err != nil {
				return errors.Wrap(err, "exporting seed data")
			}
			body, err = ioutil.ReadAll(bodyReader)
			if err != nil {
				return err
			}
		} else {
			const defaultText = ""
			var err error
			body, err = editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, defaultText, "sql")
			if err != nil {
				return errors.Wrap(err, "cannot find default editor from env")
			}
		}
		createSeedOpts.Data = bytes.NewReader(body)
	}

	fs := afero.NewOsFs()
	filepath, err := seed.CreateSeedFile(fs, createSeedOpts)
	if err != nil || filepath == nil {
		return errors.Wrap(err, "failed to create seed file")
	}

	o.FilePath = *filepath

	return nil
}
