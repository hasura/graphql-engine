package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins/index"
	"github.com/hasura/graphql-engine/cli/plugins/installation"
	"github.com/hasura/graphql-engine/cli/plugins/types"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsInstallCmd := &cobra.Command{
		Use:          "install",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.MinimumNArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			// Check if plugins index exists else create it
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			var pluginNames = make([]string, len(args))
			copy(pluginNames, args)

			var install []types.Plugin
			for _, name := range pluginNames {
				plugin, err := index.LoadPluginByName(ec.PluginsPath.IndexPluginsPath(), name)
				if err != nil {
					if os.IsNotExist(err) {
						return errors.Errorf("plugin %q does not exist in the plugin index", name)
					}
					return errors.Wrapf(err, "failed to load plugin %q from the index", name)
				}
				install = append(install, plugin)
			}

			var failed []string
			var returnErr error
			for _, plugin := range install {
				err := installation.Install(ec.PluginsPath, plugin)
				if err == installation.ErrIsAlreadyInstalled {
					continue
				}
				if err != nil {
					if returnErr == nil {
						returnErr = err
					}
					failed = append(failed, plugin.Name)
					continue
				}
				output := fmt.Sprintf("Use this plugin:\n\tkubectl %s\n", plugin.Name)
				if plugin.Homepage != "" {
					output += fmt.Sprintf("Documentation:\n\t%s\n", plugin.Homepage)
				}
				fmt.Fprintln(os.Stderr, indent(output))
			}
			if len(failed) > 0 {
				return errors.Wrapf(returnErr, "failed to install some plugins: %+v", failed)
			}
			return nil
		},
	}
	return pluginsInstallCmd
}
