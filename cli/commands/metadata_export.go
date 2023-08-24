package commands

import (
	"fmt"
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/spf13/cobra"
)

const longHelpMetadataExportCmd = `Export Hasura Metadata and save it in the` + " ``/metadata``" + ` directory.
The output is a collection of yaml files which captures all the Metadata required
by the GraphQL Engine. This includes info about tables that are tracked,
permission rules, relationships, and event triggers that are defined
on those tables.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-metadata/
- https://hasura.io/docs/latest/migrations-metadata-seeds/metadata-format/
`

func newMetadataExportCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataExportOptions{
		EC: ec,
	}

	metadataExportCmd := &cobra.Command{
		Use:   "export",
		Short: "Export Hasura GraphQL Engine Metadata from the database",
		Example: `  # Export metadata and save it in migrations/metadata.yaml file:
  hasura metadata export

  # Use with admin secret:
  hasura metadata export --admin-secret "<admin-secret>"

  # Export metadata from another instance specified by the flag:
  hasura metadata export --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if len(opts.output) == 0 {
				ec.Spin("Exporting metadata...")
			}
			err := opts.Run()
			ec.Spinner.Stop()
			if err != nil {
				return errors.E(op, err)
			}
			if len(opts.output) == 0 {
				ec.Logger.Info("Metadata exported")
			}
			return nil
		},
		Long: longHelpMetadataExportCmd,
	}

	f := metadataExportCmd.Flags()
	f.StringVarP(&opts.output, "output", "o", "", `write metadata to standard output in given format for exported metadata (note: this won't modify project metadata) Allowed values: json, yaml")`)

	return metadataExportCmd
}

type MetadataExportOptions struct {
	EC *cli.ExecutionContext

	output string
}

func (o *MetadataExportOptions) Run() error {
	var op errors.Op = "commands.MetadataExportOptions.Run"
	if len(o.output) != 0 {
		if err := getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.output)); err != nil {
			return errors.E(op, err)
		}
		return nil
	}
	if err := getMetadataModeHandler(o.EC.MetadataMode).Export(o); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func getMetadataFromServerAndWriteToStdoutByFormat(ec *cli.ExecutionContext, format rawOutputFormat) error {
	var op errors.Op = "commands.getMetadataFromServerAndWriteToStdoutByFormat"
	metadataReader, err := cli.GetCommonMetadataOps(ec).ExportMetadata()
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to export metadata: %w", err))
	}

	jsonMetadata, err := ioutil.ReadAll(metadataReader)
	if err != nil {
		return errors.E(op, fmt.Errorf("reading metadata failed: %w", err))
	}
	if err := writeByOutputFormat(ec.Stdout, jsonMetadata, format); err != nil {
		return errors.E(op, err)
	}
	return nil
}
