package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const longHelpMetadataExportCmd = `Export Hasura metadata and save it in migrations/metadata.yaml file.
The output is a yaml file which captures all the metadata required
by GraphQL Engine. This includes info about tables that are tracked,
permission rules, relationships and event triggers that are defined
on those tables.`

func newMetadataExportCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataExportOptions{
		EC:         ec,
		ActionType: "export",
	}

	metadataExportCmd := &cobra.Command{
		Use:   "export",
		Short: "Export Hasura GraphQL Engine metadata from the database",
		Example: `  # Export metadata and save it in migrations/metadata.yaml file:
  hasura metadata export

  # Use with admin secret:
  hasura metadata export --admin-secret "<admin-secret>"

  # Export metadata to another instance specified by the flag:
  hasura metadata export --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Exporting metadata...")
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to export metadata")
			}
			opts.EC.Logger.Info("Metadata exported")
			return nil
		},
		Long: longHelpMetadataExportCmd,
	}

	return metadataExportCmd
}

type MetadataExportOptions struct {
	EC *cli.ExecutionContext

	ActionType string
}

func (o *MetadataExportOptions) Run() error {
	migrateDrv, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return err
	}
	return executeMetadata(o.ActionType, migrateDrv, o.EC)
}
