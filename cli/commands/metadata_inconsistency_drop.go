package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataInconsistencyDropCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataInconsistencyDropOptions{
		EC: ec,
	}
	metadataInconsistencyDropCmd := &cobra.Command{
		Use:          "drop",
		Short:        "Drop inconsistent objects from the metadata",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("Dropping inconsistent metadata...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to drop inconsistent metadata")
			}
			opts.EC.Logger.Info("all inconsistent objects removed from metadata")
			return nil
		},
	}

	return metadataInconsistencyDropCmd
}

type metadataInconsistencyDropOptions struct {
	EC *cli.ExecutionContext
}

func (o *metadataInconsistencyDropOptions) run() error {
	d, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	return d.DropInconsistentMetadata()
}
