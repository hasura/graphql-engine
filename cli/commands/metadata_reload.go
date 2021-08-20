package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataReloadCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataReloadOptions{
		EC: ec,
	}

	metadataReloadCmd := &cobra.Command{
		Use:   "reload",
		Short: "Reload Hasura GraphQL engine metadata on the database",
		Example: `  # Reload all the metadata information from database:
  hasura metadata reload

  # Use with admin secret:
  hasura metadata reload --admin-secret "<admin-secret>"

  # Reload metadata on a different instance:
  hasura metadata export --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.runWithInfo()
		},
	}

	return metadataReloadCmd
}

type MetadataReloadOptions struct {
	EC *cli.ExecutionContext
}

func (o *MetadataReloadOptions) runWithInfo() error {
	o.EC.Spin("Reloading metadata...")
	err := o.run()
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.Wrap(err, "failed to reload metadata")
	}
	o.EC.Logger.Info("Metadata reloaded")
	return nil
}

func (o *MetadataReloadOptions) run() error {
	var err error
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	_, err = metadataHandler.ReloadMetadata()
	if err != nil {
		return errors.Wrap(err, "Cannot reload metadata")
	}
	return nil
}
