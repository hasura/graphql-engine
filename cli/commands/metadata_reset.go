package commands

import (
	"net/url"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func NewMetadataResetCmd(ec *cli.ExecutionContext) *cobra.Command {
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
			err := ec.Validate()
			if err != nil {
				return errors.Wrap(err, "validation failed")
			}
			return opts.Run()
		},
	}

	return metadataResetCmd
}

type metadataResetOptions struct {
	EC *cli.ExecutionContext

	actionType string
}

func (o *metadataResetOptions) Run() error {
	dbURL, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "error parsing Endpoint")
	}

	dbURL.Scheme = "hasuradb"
	dbURL.User = url.UserPassword("admin", o.EC.Config.AccessKey)
	err = util.ExecuteMetadata(o.actionType, "file://"+o.EC.MigrationDir, dbURL.String(), o.EC.ExecutionDirectory)
	if err != nil {
		return errors.Wrap(err, "Cannot reset metadata")
	}
	return nil
}
