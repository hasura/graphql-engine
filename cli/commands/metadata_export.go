package commands

import (
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const longHelpMetadataExportCmd = `Export Hasura metadata and save it in the` + " ``/metadata``" + ` directory.
The output is a bunch of yaml files which captures all the metadata required
by the GraphQL engine. This includes info about tables that are tracked,
permission rules, relationships and event triggers that are defined
on those tables`

func newMetadataExportCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataExportOptions{
		EC: ec,
	}

	metadataExportCmd := &cobra.Command{
		Use:   "export",
		Short: "Export Hasura GraphQL engine metadata from the database",
		Example: `  # Export metadata and save it in migrations/metadata.yaml file:
  hasura metadata export

  # Use with admin secret:
  hasura metadata export --admin-secret "<admin-secret>"

  # Export metadata to another instance specified by the flag:
  hasura metadata export --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(opts.output) == 0 {
				ec.Spin("Exporting metadata...")
			}
			err := opts.Run()
			ec.Spinner.Stop()
			if err != nil {
				return err
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
	if len(o.output) != 0 {
		return getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.output))
	}
	return getMetadataModeHandler(o.EC.MetadataMode).Export(o)
}

func getMetadataFromServerAndWriteToStdoutByFormat(ec *cli.ExecutionContext, format rawOutputFormat) error {
	metadataReader, err := cli.GetCommonMetadataOps(ec).ExportMetadata()
	if err != nil {
		return errors.Wrap(err, "failed to export metadata")
	}

	jsonMetadata, err := ioutil.ReadAll(metadataReader)
	if err != nil {
		return errors.Wrap(err, "reading metadata failed")
	}
	return writeByOutputFormat(ec.Stdout, jsonMetadata, format)
}
