package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &PluginInstallOptions{
		EC: ec,
	}
	pluginsInstallCmd := &cobra.Command{
		Use:          "install",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.Name = args[0]
			ec.Spin(fmt.Sprintf("Installing plugin %q...", opts.Name))
			defer ec.Spinner.Stop()
			err := opts.Run()
			if err == plugins.ErrIsAlreadyInstalled {
				ec.Spinner.Stop()
				ec.Logger.WithField("name", opts.Name).Infof("%q", err)
				return nil
			}
			if err != nil && err != plugins.ErrIsAlreadyInstalled {
				return errors.Wrapf(err, "failed to install plugin %q", opts.Name)
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("name", opts.Name).Infoln("plugin installed")
			return nil
		},
	}

	f := pluginsInstallCmd.Flags()

	f.StringVar(&opts.ManifestFile, "manifest-file", "", "(dev) speficy local manifest file")
	return pluginsInstallCmd
}

type PluginInstallOptions struct {
	EC *cli.ExecutionContext

	Name         string
	ManifestFile string
}

func (o *PluginInstallOptions) Run() error {
	return ec.Plugins.Install(o.Name, o.ManifestFile)
}
