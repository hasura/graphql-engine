package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataResetCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataResetOptions{
		EC:         ec,
		actionType: "reset",
	}

	metadataResetCmd := &cobra.Command{
		Use:   "reset",
		Short: "Reset or clean Hasura GraphQL Engine metadata on the database",
		Example: `  # Clean all the metadata information from database:
  hasura metadata reset`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	return metadataResetCmd
}

type metadataResetOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataResetOptions) run() error {
	dbURL := util.GetDataPath(o.EC.Config.ParsedEndpoint, o.EC.Config.AccessKey)
	err := util.ExecuteMetadata(o.actionType, "file://"+o.EC.MigrationDir, dbURL, o.EC.ExecutionDirectory)
	if err != nil {
		return errors.Wrap(err, "Cannot reset metadata")
	}
	return nil
}
