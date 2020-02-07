package commands

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
		Use:          "upgrade",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			pluginName := args[0]
			ec.Spin(fmt.Sprintf("Upgrading plugin %q...", pluginName))
			defer ec.Spinner.Stop()
			plugin, err := ec.Plugins.Upgrade(pluginName)
			if err != nil && err != plugins.ErrIsAlreadyUpgraded {
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
