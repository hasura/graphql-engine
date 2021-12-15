package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

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
		return fmt.Errorf("failed to reload metadata: %w", err)
	}
	o.EC.Logger.Info("Metadata reloaded")
	icListOpts := &metadataInconsistencyListOptions{
		EC: o.EC,
	}
	err = icListOpts.read(projectmetadata.NewHandlerFromEC(icListOpts.EC))
	if err != nil {
		return fmt.Errorf("failed to read metadata status: %w", err)
	}
	if icListOpts.isConsistent {
		icListOpts.EC.Logger.Infoln("Metadata is consistent")
	} else {
		icListOpts.EC.Logger.Warnln("Metadata is inconsistent, use 'hasura metadata ic list' command to see the inconsistent objects")
	}
	return nil
}

func (o *MetadataReloadOptions) run() error {
	var err error
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	_, err = metadataHandler.ReloadMetadata()
	if err != nil {
		return fmt.Errorf("cannot reload metadata: %w", err)
	}
	return nil
}
