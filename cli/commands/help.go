package commands

import (
	"fmt"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
)

type commandGroup struct {
	Title    string
	Commands []*cobra.Command
}

// NewHelpCmd returns the help command
func NewHelpCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &helpOptions{
		EC: ec,
	}
	var helpCmd = &cobra.Command{
		Use:   "help",
		Short: "Help about any command",
		Long:  "Help provides help for any command in the CLI",
		Run: func(cmd *cobra.Command, args []string) {
			opts.Cmd = cmd
			opts.Args = args
			opts.run()
		},
	}
	return helpCmd
}

type helpOptions struct {
	EC *cli.ExecutionContext

	Cmd  *cobra.Command
	Args []string
}

func (o *helpOptions) run() {
	topLevelCommands := []commandGroup{
		{
			Title: "GraphQL commands",
			Commands: []*cobra.Command{
				NewInitCmd(o.EC),
				NewMigrateCmd(o.EC),
				NewMetadataCmd(o.EC),
				NewConsoleCmd(o.EC),
				NewActionsCmd(o.EC),
				NewSeedCmd(o.EC),
				NewDeployCmd(o.EC),
			},
		},
		{
			Title: "Other commands",
			Commands: []*cobra.Command{
				NewCompletionCmd(o.EC),
				NewVersionCmd(o.EC),
				NewPluginsCmd(o.EC),
				NewScriptsCmd(o.EC),
				NewUpdateCLICmd(o.EC),
			},
		},
	}
	c := o.Cmd
	args := o.Args
	cmd, _, e := c.Root().Find(args)
	if cmd == nil || e != nil {
		c.Printf("Unknown help topic %#q\n", args)
		err := c.Root().Usage()
		if err != nil {
			ec.Logger.WithError(err).Errorf("error while using a dependency library")
		}
	} else {
		if cmd.Name() == "hasura" {
			// root command
			fmt.Println(cmd.Long)
			w := tabwriter.NewWriter(o.EC.Stdout, 0, 0, 3, ' ', 0)
			for _, g := range topLevelCommands {
				fmt.Println(g.Title + ":")
				for _, gc := range g.Commands {
					fmt.Fprintf(w, "  %s\t%s\n", gc.Name(), gc.Short)
				}
				w.Flush()
				fmt.Println("")
			}
			fmt.Println(`Use "hasura [command] --help" for more information about a command.`)
		} else {
			cmd.InitDefaultHelpFlag() // make possible 'help' flag to be shown
			err := cmd.Help()
			if err != nil {
				ec.Logger.WithError(err).Errorf("error while using a dependency library")
			}
		}
	}

}
