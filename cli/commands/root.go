package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:           "hasura",
	Short:         "Hasura GraphQL Engine command line tool",
	SilenceUsage:  true,
	SilenceErrors: true,
}

func init() {
	ec := &cli.ExecutionContext{}
	rootCmd.AddCommand(
		NewInitCmd(ec),
		NewConsoleCmd(ec),
		NewMetadataCmd(ec),
		NewMigrateCmd(ec),
	)
	f := rootCmd.PersistentFlags()
	f.StringVar(&ec.LogLevel, "log-level", "INFO", "log level (DEBUG, INFO, WARN, ERROR, FATAL)")
}

func Execute() error {
	return rootCmd.Execute()
}
