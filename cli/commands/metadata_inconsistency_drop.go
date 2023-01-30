package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/spf13/cobra"
)

func newMetadataInconsistencyDropCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataInconsistencyDropOptions{
		EC: ec,
	}
	metadataInconsistencyDropCmd := &cobra.Command{
		Use:   "drop",
		Short: "Drop inconsistent objects from the Hasura Metadata",
		Long: `At times, when developing, the Hasura Metadata can become inconsistent. This command can be used to drop inconsistent objects from the Hasura Metadata and bring your project's Metadata back to a consistent state.
		
Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/resetting-migrations-metadata/
`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.EC.Spin("Dropping inconsistent metadata...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.E(op, fmt.Errorf("failed to drop inconsistent metadata: %w", err))
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
	var op errors.Op = "commands.metadataInconsistencyDropOptions.run"
	if err := projectmetadata.NewHandlerFromEC(o.EC).DropInconsistentMetadata(); err != nil {
		return errors.E(op, err)
	}
	return nil
}
