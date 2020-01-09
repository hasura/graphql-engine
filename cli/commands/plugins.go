package commands

import (
	"os"
	"regexp"
	"strings"
	"unicode"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func NewPluginsCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsCmd := &cobra.Command{
		Use:          "plugins",
		Short:        "",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			return ensureDirs(ec.PluginsPath.BasePath(),
				ec.PluginsPath.DownloadPath(),
				ec.PluginsPath.InstallPath(),
				ec.PluginsPath.BinPath(),
				ec.PluginsPath.InstallReceiptsPath(),
			)
		},
	}
	pluginsCmd.AddCommand(
		newPluginsListCmd(ec),
		newPluginsInstallCmd(ec),
		newPluginsUnInstallCmd(ec),
	)
	return pluginsCmd
}

func ensureDirs(paths ...string) error {
	for _, p := range paths {
		if err := os.MkdirAll(p, 0755); err != nil {
			return errors.Wrapf(err, "failed to ensure create directory %q", p)
		}
	}
	return nil
}

func indent(s string) string {
	out := "\\\n"
	s = strings.TrimRightFunc(s, unicode.IsSpace)
	out += regexp.MustCompile("(?m)^").ReplaceAllString(s, " | ")
	out += "\n/"
	return out
}
