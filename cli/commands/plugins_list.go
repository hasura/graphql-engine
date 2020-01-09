package commands

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
	"text/tabwriter"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins/installation"
)

func newPluginsListCmd(ec *cli.ExecutionContext) *cobra.Command {
	pluginsListCmd := &cobra.Command{
		Use:          "list",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			installedPlugins, err := installation.ListInstalledPlugins(ec.PluginsPath.InstallReceiptsPath())
			if err != nil {
				return errors.Wrap(err, "failed to find all installed versions")
			}

			// print installed plugins
			// TODO: print if update is available
			var rows [][]string
			for p, version := range installedPlugins {
				rows = append(rows, []string{p, version})
			}
			rows = sortByFirstColumn(rows)
			return printTable(os.Stdout, []string{"PLUGIN", "VERSION"}, rows)

			// TODO: print plugins available to install
		},
	}
	return pluginsListCmd
}

func printTable(out io.Writer, columns []string, rows [][]string) error {
	w := tabwriter.NewWriter(out, 0, 0, 2, ' ', 0)
	fmt.Fprint(w, strings.Join(columns, "\t"))
	fmt.Fprintln(w)
	for _, values := range rows {
		fmt.Fprint(w, strings.Join(values, "\t"))
		fmt.Fprintln(w)
	}
	return w.Flush()
}

func sortByFirstColumn(rows [][]string) [][]string {
	sort.Slice(rows, func(a, b int) bool {
		return rows[a][0] < rows[b][0]
	})
	return rows
}
