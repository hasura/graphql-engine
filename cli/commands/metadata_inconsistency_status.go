package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataInconsistencyStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataInconsistencyListOptions{
		EC: ec,
	}

	metadataInconsistencyStatusCmd := &cobra.Command{
		Use:          "status",
		Short:        "Check if the metadata is inconsistent or not",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("reading metadata status...")
			err := opts.read()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to read metadata status")
			}
			if opts.isConsistent {
				opts.EC.Logger.Println("metadata is consistent")
			} else {
				return errors.New("metadata is inconsistent, use list command to see the objects")
			}
			return nil
		},
	}

	return metadataInconsistencyStatusCmd
}
