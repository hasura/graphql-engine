package commands

import (
	"fmt"
	"io"
	"os"
	"runtime"
	"sort"
	"strings"
	"text/tabwriter"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins"
)

func newPluginsListCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &pluginListOptions{
		EC: ec,
	}
	pluginsListCmd := &cobra.Command{
		Use:          "list",
		Aliases:      []string{"ls"},
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := pluginsListCmd.Flags()
	f.BoolVar(&opts.updateIndex, "update-index", true, "update plugin index")

	return pluginsListCmd
}

type pluginListOptions struct {
	EC *cli.ExecutionContext

	updateIndex bool
}

func (p *pluginListOptions) run() error {
	if p.updateIndex {
		ec.Spin("Updating plugin index...")
		err := p.EC.PluginsConfig.Repo.EnsureUpdated()
		if err != nil {
			p.EC.Logger.Warnf("unable to update plugin index %q", err)
		}
	}
	ec.Spin("Fetching plugins list...")
	defer ec.Spinner.Stop()
	availablePlugins, err := ec.PluginsConfig.ListPlugins()
	if err != nil {
		return errors.Wrap(err, "failed to load the list of plugins from the index")
	}
	names := make([]string, len(availablePlugins))
	pluginMap := make(map[string]plugins.Plugin, len(availablePlugins))
	for i, p := range availablePlugins {
		names[i] = p.Name
		pluginMap[p.Name] = p
	}
	installed, err := ec.PluginsConfig.ListInstalledPlugins()
	if err != nil {
		return errors.Wrap(err, "failed to load installed plugins")
	}
	// No plugins found
	if len(names) == 0 {
		return nil
	}
	var rows [][]string
	cols := []string{"NAME", "DESCRIPTION", "VERSION", "INSTALLED"}
	for _, name := range names {
		plugin := pluginMap[name]
		var status string
		var version string
		if _, ok := installed[name]; ok {
			status = "yes"
			version = installed[name]
		} else if _, ok, err := plugins.MatchPlatform(plugin.Platforms); err != nil {
			return errors.Wrapf(err, "failed to get the matching platform for plugin %s", name)
		} else if ok {
			status = "no"
		} else {
			status = "unavailable on " + runtime.GOOS
		}
		if status == "yes" {
			version = installed[name]
		} else {
			version = plugin.Version
		}
		rows = append(rows, []string{name, limitString(plugin.ShortDescription, 50), version, status})
	}
	rows = sortByFirstColumn(rows)
	ec.Spinner.Stop()
	return printTable(os.Stdout, cols, rows)
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
