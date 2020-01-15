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

func newPluginsInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsInstallCmd := &cobra.Command{
		Use:          "install",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			pluginName := args[0]
			ec.Spin(fmt.Sprintf("Installing plugin %q...", pluginName))
			defer ec.Spinner.Stop()
			plugin, err := index.LoadPluginByName(ec.PluginsPath.IndexPluginsPath(), pluginName)
			if err != nil {
				if os.IsNotExist(err) {
					return errors.Errorf("plugin %q does not exist in the plugin index", pluginName)
				}
				return errors.Wrapf(err, "failed to load plugin %q from the index", pluginName)
			}
			err = installation.Install(ec.PluginsPath, plugin)
			if err != nil && err != installation.ErrIsAlreadyInstalled {
				return errors.Wrapf(err, "failed to install plugin %q", pluginName)
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("name", pluginName).Infoln("plugin installed")
			output := fmt.Sprintf("Use this plugin:\n\thasura %s\n", plugin.Name)
			if plugin.Homepage != "" {
				output += fmt.Sprintf("Documentation:\n\t%s\n", plugin.Homepage)
			}
			fmt.Println(indent(output))
			return nil
		},
	}
	return pluginsInstallCmd
}
