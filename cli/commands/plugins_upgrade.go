package commands

import (
	"fmt"
	"os"

	"github.com/sirupsen/logrus"

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
		Args:         cobra.MaximumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			pluginName := args[0]
			ec.Spin(fmt.Sprintf("Upgrading plugin %q...", pluginName))
			defer ec.Spinner.Stop()
			plugin, err := index.LoadPluginByName(ec.PluginsPath.IndexPluginsPath(), pluginName)
			if err != nil {
				if os.IsNotExist(err) {
					return errors.Errorf("plugin %q does not exist in the plugin index", pluginName)
				}
				return errors.Wrapf(err, "failed to load the plugin manifest for plugin %s", pluginName)
			}

			err = installation.Upgrade(ec.PluginsPath, plugin)
			if err != nil && err != installation.ErrIsAlreadyUpgraded {
				return errors.Wrapf(err, "failed to upgrade plugin %q", plugin.Name)
			}
			ec.Spinner.Stop()
			ec.Logger.WithFields(logrus.Fields{
				"name":    pluginName,
				"version": plugin.Version,
			}).Infoln("Upgraded plugin")
			return nil
		},
	}
	return pluginsUpgradeCmd
}
