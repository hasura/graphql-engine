package commands

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/blob/master/cmd/krew/cmd/upgrade.go
*/

import (
	"fmt"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/plugins"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/spf13/cobra"
)

func newPluginsUpgradeCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &PluginUpgradeOptions{
		EC: ec,
	}
	pluginsUpgradeCmd := &cobra.Command{
		Use:   "upgrade",
		Short: "Upgrade a plugin to a newer version",
		Long:  "To upgrade a plugin, run the upgrade command with the name of the plugin as an argument. If unsure of the plugin's name, you can run the `Hasura plugins list` command to see a list of all the available plugins.",
		Example: `  # Upgrade a plugin to a newer version
  hasura plugins upgrade [plugin-name]`,
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
			opts.Name = args[0]
			ec.Spin(fmt.Sprintf("Upgrading plugin %q...", opts.Name))
			defer ec.Spinner.Stop()
			plugin, err := opts.Run()
			if err != nil && err != plugins.ErrIsAlreadyUpgraded {
				return errors.E(op, fmt.Errorf("failed to upgrade plugin %q: %w", opts.Name, err))
			}
			ec.Spinner.Stop()
			ec.Logger.WithFields(logrus.Fields{
				"name":    opts.Name,
				"version": plugin.Version,
			}).Infoln("Plugin upgraded")
			return nil
		},
	}

	f := pluginsUpgradeCmd.Flags()

	f.Var(&opts.Version, "version", "version to be upgraded")

	return pluginsUpgradeCmd
}

type PluginUpgradeOptions struct {
	EC *cli.ExecutionContext

	Name    string
	Version util.VersionFlag
}

func (o *PluginUpgradeOptions) Run() (plugins.Plugin, error) {
	var op errors.Op = "commands.PluginUpgradeOptions.Run"
	p, err := o.EC.PluginsConfig.Upgrade(o.Name, o.Version.Version)
	if err != nil {
		return p, errors.E(op, err)
	}
	return p, nil
}
