package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins/gitutil"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsUpdateCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsUpdateCmd := &cobra.Command{
		Use:          "update",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			if err := gitutil.EnsureUpdated(ec.PluginsPath.IndexPath()); err != nil {
				return errors.Wrap(err, "failed to update the local index")
			}
			return nil
		},
	}
	return pluginsUpdateCmd
}
