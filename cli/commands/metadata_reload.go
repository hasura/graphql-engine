package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/spf13/cobra"
)

func newMetadataReloadCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataReloadOptions{
		EC: ec,
	}

	metadataReloadCmd := &cobra.Command{
		Use:   "reload",
		Short: "Reload Hasura GraphQL Engine schema to pick up changes in any underlying data sources (database or remote schema)",
		Long:  `hasura metadata reload should be used when there is a change in the underlying data sources (database or remote schema) that Hasura should be aware of.
Example: 
  A new column is added to a table and this column should now be added to the GraphQL schema.`,
		Example: `  # Reload all the metadata information from database:
  hasura metadata reload

  # Use with admin secret:
  hasura metadata reload --admin-secret "<admin-secret>"

  # Use with a specific endpoint:
  hasura metadata reload --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if err := opts.runWithInfo(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	return metadataReloadCmd
}

type MetadataReloadOptions struct {
	EC *cli.ExecutionContext
}

func (o *MetadataReloadOptions) runWithInfo() error {
	var op errors.Op = "commands.MetadataReloadOptions.runWithInfo"
	o.EC.Spin("Reloading metadata...")
	err := o.run()
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to reload metadata: %w", err))
	}
	o.EC.Logger.Info("Metadata reloaded")
	icListOpts := &metadataInconsistencyListOptions{
		EC: o.EC,
	}
	err = icListOpts.read(projectmetadata.NewHandlerFromEC(icListOpts.EC))
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to read metadata status: %w", err))
	}
	if icListOpts.isConsistent {
		icListOpts.EC.Logger.Infoln("Metadata is consistent")
	} else {
		icListOpts.EC.Logger.Warnln("Metadata is inconsistent, use 'hasura metadata ic list' command to see the inconsistent objects")
	}
	return nil
}

func (o *MetadataReloadOptions) run() error {
	var op errors.Op = "commands.MetadataReloadOptions.run"
	var err error
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	_, err = metadataHandler.ReloadMetadata()
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot reload metadata: %w", err))
	}
	return nil
}
