package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins/installation"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newPluginsUnInstallCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsUnInstallCmd := &cobra.Command{
		Use:          "uninstall",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		Args:         cobra.MinimumNArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			for _, name := range args {
				if err := installation.Uninstall(ec.PluginsPath, name); err != nil {
					return errors.Wrapf(err, "failed to uninstall plugin %s", name)
				}
				fmt.Fprintf(os.Stderr, "Uninstalled plugin %s\n", name)
			}
			return nil
		},
	}
	return pluginsUnInstallCmd
}
