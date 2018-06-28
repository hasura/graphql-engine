package commands

import (
	"net/url"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func NewMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
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
	dbURL, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "error parsing Endpoint")
	}

	dbURL.Scheme = "hasuradb"
	dbURL.User = url.UserPassword("admin", o.EC.Config.AccessKey)
	return util.ExecuteMetadata(o.actionType, "file://"+o.EC.MigrationDir, dbURL.String(), o.EC.ExecutionDirectory)
}
