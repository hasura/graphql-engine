package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	var manifestFile *string
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
			err := ec.Plugins.Install(pluginName, *manifestFile)
			if err == plugins.ErrIsAlreadyInstalled {
				ec.Spinner.Stop()
				ec.Logger.WithField("name", pluginName).Infof("%q", err)
				return nil
			}
			if err != nil && err != plugins.ErrIsAlreadyInstalled {
				return errors.Wrapf(err, "failed to install plugin %q", pluginName)
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("name", pluginName).Infoln("plugin installed")
			return nil
		},
	}

	f := pluginsInstallCmd.Flags()

	manifestFile = f.String("manifest-file", "", "(dev) speficy local manifest file")
	return pluginsInstallCmd
}
