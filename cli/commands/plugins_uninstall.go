package commands

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/blob/master/cmd/krew/cmd/uninstall.go
*/

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"

	"github.com/spf13/cobra"
)

func newPluginsUnInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsUnInstallCmd := &cobra.Command{
		Use:   "uninstall [plugin-name]",
		Short: "Uninstall a plugin",
		Long:  "To uninstall a plugin, run the uninstall command with the name of the plugin as an argument. If unsure of the plugin's name, you can run the `Hasura plugins list` command to see a list of all the available plugins.",
		Example: `  # Uninstall a plugin
  hasura plugins uninstall [plugin-name]`,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if err := ec.Prepare(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			pluginName := args[0]
			ec.Spin(fmt.Sprintf("Uninstalling plugin %q", pluginName))
			defer ec.Spinner.Stop()
			if err := ec.PluginsConfig.Uninstall(pluginName); err != nil {
				return errors.E(op, fmt.Errorf("failed to uninstall plugin %s: %w", pluginName, err))
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("name", pluginName).Infoln("plugin uninstalled")
			return nil
		},
	}
	return pluginsUnInstallCmd
}
