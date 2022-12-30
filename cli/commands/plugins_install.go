package commands

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/blob/master/cmd/krew/cmd/install.go
*/

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/plugins"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/spf13/cobra"
)

func newPluginsInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &PluginInstallOptions{
		EC: ec,
	}
	pluginsInstallCmd := &cobra.Command{
		Use:   "install [plugin-name]",
		Short: "Install a plugin from the index",
		Long:  "To install plugins that extend the functionality of the Hasura CLI, you can use the install command. This command will install the plugin from the index and add it to your configuration file.",
		Example: `  # Install a plugin:
  hasura plugins install [plugin-name]`,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			err := ec.Prepare()
			if err != nil {
				return errors.E(op, err)
			}
			err = ec.PluginsConfig.Repo.EnsureUpdated()
			if err != nil {
				ec.Logger.Debugf("unable to update plugins index: got %v", err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
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
				return errors.E(op, fmt.Errorf("failed to install plugin %q: %w", opts.Name, err))
			}
			ec.Spinner.Stop()
			ec.Logger.WithField("name", opts.Name).Infoln("plugin installed")
			return nil
		},
	}

	f := pluginsInstallCmd.Flags()

	f.Var(&opts.Version, "version", "version to be installed")
	f.StringVar(&opts.ManifestFile, "manifest-file", "", "(dev) speficy local manifest file")
	if err := f.MarkHidden("manifest-file"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	return pluginsInstallCmd
}

type PluginInstallOptions struct {
	EC *cli.ExecutionContext

	Name         string
	ManifestFile string
	Version      util.VersionFlag
}

func (o *PluginInstallOptions) Run() error {
	var op errors.Op = "commands.PluginInstallOptions.Run"
	plugin, err := o.EC.PluginsConfig.GetPlugin(o.Name, plugins.FetchOpts{
		ManifestFile: o.ManifestFile,
		Version:      o.Version.Version,
	})
	if err != nil {
		return errors.E(op, err)
	}
	if err := o.EC.PluginsConfig.Install(plugin); err != nil {
		return errors.E(op, err)
	}
	return nil
}
