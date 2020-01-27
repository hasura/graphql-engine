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
			ec.Spin("Updating plugins index...")
			defer ec.Spinner.Stop()
			if err := gitutil.EnsureUpdated(ec.Plugins.Paths.IndexPath()); err != nil {
				return errors.Wrap(err, "failed to update the local index")
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("path", ec.Plugins.Paths.IndexPath()).Infoln("updated plugins index")
			return nil
		},
	}
	return pluginsUpdateCmd
}
