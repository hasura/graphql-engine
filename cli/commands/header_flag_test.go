package commands

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestHeaderFlagRegistration asserts that the repeatable `--header` flag is wired up
// on every command group that communicates with the Hasura GraphQL Engine, that it is
// a repeatable (stringArray) flag, and that repeated values accumulate into
// ExecutionContext.HGEHeaderFlags.
func TestHeaderFlagRegistration(t *testing.T) {
	commandGroups := []struct {
		name string
		new  func(*cli.ExecutionContext) *cobra.Command
	}{
		{"migrate", NewMigrateCmd},
		{"metadata", NewMetadataCmd},
		{"deploy", NewDeployCmd},
		{"seed", NewSeedCmd},
		{"console", NewConsoleCmd},
		{"actions", NewActionsCmd},
	}

	for _, cg := range commandGroups {
		t.Run(cg.name, func(t *testing.T) {
			ec := cli.NewExecutionContext()
			cmd := cg.new(ec)

			// cmd.LocalFlags() merges the command's own local and persistent flags, so
			// this works whether the group registers --header on Flags() (deploy,
			// console) or PersistentFlags() (migrate, metadata, seed, actions).
			flags := cmd.LocalFlags()
			flag := flags.Lookup("header")
			require.NotNil(t, flag, "--header flag should be registered on %q", cg.name)
			assert.Equal(t, "stringArray", flag.Value.Type(),
				"--header should be a repeatable (stringArray) flag")

			// Repeated use of the flag should accumulate, not overwrite.
			require.NoError(t, flags.Set("header", "Authorization: Bearer token"))
			require.NoError(t, flags.Set("header", "X-Tenant: acme"))

			assert.Equal(t,
				[]string{"Authorization: Bearer token", "X-Tenant: acme"},
				ec.HGEHeaderFlags,
			)
		})
	}
}
