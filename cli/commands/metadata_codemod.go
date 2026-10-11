package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// newMetadataCodemodCmd returns the `hasura metadata codemod` parent command,
// which groups one-off metadata migration/transformation helpers.
//
// It defines its own PersistentPreRunE (overriding the parent `metadata`
// command's) so that it initialises the execution context WITHOUT running the
// automatic config-v2-to-v3 upgrade: a codemod must never mutate the project
// before it has had a chance to validate input or honour --dry-run.
func newMetadataCodemodCmd(ec *cli.ExecutionContext, v *viper.Viper) *cobra.Command {
	metadataCodemodCmd := &cobra.Command{
		Use:          "codemod",
		Short:        "Run codemods on Hasura Metadata",
		Long:         "A collection of commands that transform your local Hasura Metadata in place.",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PersistentPreRunE")
			cmd.Root().PersistentPreRun(cmd, args)

			ec.Viper = v
			if err := ec.Prepare(); err != nil {
				return errors.E(op, err)
			}

			if err := ec.Validate(); err != nil {
				return errors.E(op, err)
			}

			return nil
		},
	}

	metadataCodemodCmd.AddCommand(
		newMetadataCodemodMigratePostgresCommentsCmd(ec),
	)

	return metadataCodemodCmd
}
