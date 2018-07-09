package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func newMetadataExportCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataExportOptions{
		EC:         ec,
		actionType: "export",
	}

	metadataExportCmd := &cobra.Command{
		Use:   "export",
		Short: "Export Hasura GraphQL Engine metadata from the database",
		Example: `  # Export metadata and save it in metadata.yaml file:
  hasura metadata export`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	return metadataExportCmd
}

type metadataExportOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataExportOptions) run() error {
	dbURL := getDataPath(o.EC.Config.ParsedEndpoint, o.EC.Config.AccessKey)
	fileURL := getFilePath(o.EC.MigrationDir)
	return executeMetadata(o.actionType, fileURL, dbURL, o.EC.ExecutionDirectory)
}
