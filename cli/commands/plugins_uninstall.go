package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsUnInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsUnInstallCmd := &cobra.Command{
		Use:          "uninstall",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			pluginName := args[0]
			ec.Spin(fmt.Sprintf("Uninstalling plugin %q", pluginName))
			defer ec.Spinner.Stop()
			if err := ec.Plugins.Uninstall(pluginName); err != nil {
				return errors.Wrapf(err, "failed to uninstall plugin %s", pluginName)
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("name", pluginName).Infoln("plugin uninstalled")
			return nil
		},
	}
	return pluginsUnInstallCmd
}
