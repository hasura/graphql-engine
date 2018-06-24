package commands

import "github.com/spf13/cobra"

var rootCmd = &cobra.Command{
	Use:           "hasura",
	Short:         "Hasura GraphQL Engine command line tool",
	SilenceUsage:  true,
	SilenceErrors: true,
}

func init() {
	rootCmd.AddCommand()
}

func Execute() error {
	return rootCmd.Execute()
}
