package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/spf13/cobra"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataApplyOptions{
		EC:         ec,
		actionType: "apply",
	}

	metadataApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply Hasura metadata on a database",
		Example: `  # Apply Hasura GraphQL Engine metadata present in metadata.yaml file:
  hasura metadata apply`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	return metadataApplyCmd
}

type metadataApplyOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataApplyOptions) run() error {
	dbURL := util.GetDataPath(o.EC.Config.ParsedEndpoint, o.EC.Config.AccessKey)
	return util.ExecuteMetadata(o.actionType, "file://"+o.EC.MigrationDir, dbURL, o.EC.ExecutionDirectory)
}
