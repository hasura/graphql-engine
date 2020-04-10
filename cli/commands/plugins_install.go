package commands

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/blob/master/cmd/krew/cmd/install.go
*/

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/util"

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
		Use:   "install [plugin-name]",
		Short: "Install a plugin from the index",
		Example: `  # Install a plugin:
  hasura plugins install [plugin-name]`,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			err := ec.Prepare()
			if err != nil {
				return err
			}
			err = ec.PluginsConfig.Repo.EnsureUpdated()
			if err != nil {
				ec.Logger.Debugf("unable to update plugins index: got %v", err)
			}
			return nil
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

	f.Var(&opts.Version, "version", "version to be installed")
	f.StringVar(&opts.ManifestFile, "manifest-file", "", "(dev) speficy local manifest file")
	f.MarkHidden("manifest-file")

	return pluginsInstallCmd
}

type PluginInstallOptions struct {
	EC *cli.ExecutionContext

	Name         string
	ManifestFile string
	Version      util.VersionFlag
}

func (o *PluginInstallOptions) Run() error {
	return o.EC.PluginsConfig.Install(o.Name, o.ManifestFile, o.Version.Version)
}
