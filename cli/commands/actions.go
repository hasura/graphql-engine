package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli/plugins/index"
	"github.com/hasura/graphql-engine/cli/plugins/installation"
	"github.com/pkg/errors"

	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

func NewActionsCmd(ec *cli.ExecutionContext) *cobra.Command {
	actionsCmd := &cobra.Command{
		Use:          "actions",
		Short:        "",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			pluginName := "cli-ext"
			plugin, err := index.LoadPluginByName(ec.PluginsPath.IndexPluginsPath(), pluginName)
			if err != nil {
				if os.IsNotExist(err) {
					return errors.Errorf("plugin %q does not exist in the plugin index", pluginName)
				}
				return errors.Wrapf(err, "failed to load plugin %q from the index", pluginName)
			}
			err = installation.Install(ec.PluginsPath, plugin)
			if err == installation.ErrIsAlreadyInstalled {
				return nil
			}
			return err
		},
	}
	actionsCmd.AddCommand(
		newActionsCreateCmd(ec),
	)
	return actionsCmd
}
