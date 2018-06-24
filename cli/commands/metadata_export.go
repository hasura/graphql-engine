package commands

import (
	"net/url"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func NewMetadataExportCmd(ec *cli.ExecutionContext) *cobra.Command {
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
			err := ec.Validate()
			if err != nil {
				return errors.Wrap(err, "validation failed")
			}
			return opts.Run()
		},
	}

	return metadataExportCmd
}

type metadataExportOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataExportOptions) Run() error {
	dbURL, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "error parsing Endpoint")
	}

	dbURL.Scheme = "hasuradb"
	dbURL.User = url.UserPassword("admin", o.EC.Config.AccessKey)
	return util.ExecuteMetadata(o.actionType, "file://"+o.EC.MigrationDir, dbURL.String(), o.EC.ExecutionDirectory)
}
