package commands

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/blob/master/cmd/krew/cmd/upgrade.go
*/

import (
	"fmt"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsUpgradeCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsUpgradeCmd := &cobra.Command{
		Use:   "upgrade",
		Short: "Upgrade a plugin to a newer version",
		Example: `  # Upgrade a plugin to a newer version
  hasura plugins upgrade [plugin-name]`,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			pluginName := args[0]
			ec.Spin(fmt.Sprintf("Upgrading plugin %q...", pluginName))
			defer ec.Spinner.Stop()
			plugin, err := ec.PluginsConfig.Upgrade(pluginName)
			if err != nil && err != plugins.ErrIsAlreadyUpgraded {
				return errors.Wrapf(err, "failed to upgrade plugin %q", plugin.Name)
			}
			ec.Spinner.Stop()
			ec.Logger.WithFields(logrus.Fields{
				"name":    pluginName,
				"version": plugin.Version,
			}).Infoln("Plugin upgraded")
			return nil
		},
	}
	return pluginsUpgradeCmd
}
