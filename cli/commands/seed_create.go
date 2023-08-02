package commands

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
		Long: `This will create a new seed file with the name provided as an argument. You can export tables from the database and create a seed file from it by using the ` + "``--from-table``" + ` flag.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-seeds/
`,
		Example: `  # Create a new seed file and use editor to add SQL:
  hasura seed create new_table_seed

  # Create a new seed by exporting data from tables already present in the database:
  hasura seed create table1_seed --from-table table1

  # Export data from multiple tables:
  hasura seed create tables_seed --from-table table1 --from-table table2`,
		Args:         cobra.ExactArgs(1),
		SilenceUsage: false,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if err := validateConfigV3Prechecks(cmd, ec); err != nil {
				return errors.E(op, err)
			}
			if ec.Config.Version < cli.V3 {
				return nil
			}

			if err := databaseChooser(ec); err != nil {
				return errors.E(op, err)
			}

			if err := validateSourceInfo(ec); err != nil {
				return errors.E(op, err)
			}
			// check if seed ops are supported for the database
			if !seed.IsSeedsSupported(ec.Source.Kind) {
				return errors.E(op, fmt.Errorf("seed operations on database '%s' of kind '%s' is not supported", ec.Source.Name, ec.Source.Kind))
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.SeedName = args[0]
			opts.Source = ec.Source
			opts.Driver = getSeedDriver(ec, ec.Config.Version)
			err := opts.Run()
			if err != nil {
				return errors.E(op, err)
			}
			ec.Logger.WithField("file", opts.FilePath).Info("created seed file successfully")
			return nil
		},
	}

	cmd.Flags().StringArrayVar(&opts.FromTableNames, "from-table", []string{}, "name of table from which seed file has to be initialized. e.g. table1, myschema1.table1")

	return cmd
}

func (o *SeedNewOptions) Run() error {
	var op errors.Op = "commands.SeedNewOptions.Run"
	databaseDirectory := filepath.Join(o.EC.SeedsDirectory, o.Source.Name)
	if f, _ := os.Stat(databaseDirectory); f == nil {
		if err := os.MkdirAll(databaseDirectory, 0755); err != nil {
			return errors.E(op, err)
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
				return errors.E(op, "--from-table is supported only for postgres databases")
			}
			// Send the query
			bodyReader, err := o.Driver.ExportDatadump(o.FromTableNames, o.Source.Name)
			if err != nil {
				return errors.E(op, fmt.Errorf("exporting seed data: %w", err))
			}
			body, err = ioutil.ReadAll(bodyReader)
			if err != nil {
				return errors.E(op, err)
			}
		} else {
			const defaultText = ""
			var err error
			body, err = editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, defaultText, "sql")
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot find default editor from env: %w", err))
			}
		}
		createSeedOpts.Data = bytes.NewReader(body)
	}

	fs := afero.NewOsFs()
	filepath, err := seed.CreateSeedFile(fs, createSeedOpts)
	if err != nil || filepath == nil {
		return errors.E(op, fmt.Errorf("failed to create seed file: %w", err))
	}

	o.FilePath = *filepath

	return nil
}
