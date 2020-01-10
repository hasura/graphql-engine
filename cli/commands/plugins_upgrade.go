package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins/index"
	"github.com/hasura/graphql-engine/cli/plugins/installation"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsUpgradeCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsUpgradeCmd := &cobra.Command{
		Use:          "upgrade",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.MinimumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			var nErrors int
			for _, name := range args {
				plugin, err := index.LoadPluginByName(ec.PluginsPath.IndexPluginsPath(), name)
				if err != nil {
					if os.IsNotExist(err) {
						return errors.Errorf("plugin %q does not exist in the plugin index", name)
					}
					return errors.Wrapf(err, "failed to load the plugin manifest for plugin %s", name)
				}

				err = installation.Upgrade(ec.PluginsPath, plugin)
				if err == installation.ErrIsAlreadyUpgraded {
					fmt.Fprintf(os.Stderr, "Skipping plugin %s, it is already on the newest version\n", plugin.Name)
					continue
				}

				if err != nil {
					nErrors++
					return errors.Wrapf(err, "failed to upgrade plugin %q", plugin.Name)
				}
				fmt.Fprintf(os.Stderr, "Upgraded plugin: %s\n", plugin.Name)
			}
			if nErrors > 0 {
				fmt.Fprintf(os.Stderr, "WARNING: Some plugins failed to upgrade, check logs above.\n")
			}
			return nil
		},
	}
	return pluginsUpgradeCmd
}
