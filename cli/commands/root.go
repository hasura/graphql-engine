// Package commands contains the definition for all the commands present in
// Hasura CLI.
package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/spf13/cobra"
)

// rootCmd is the main "hasura" command
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
		NewVersionCmd(ec),
		NewDocsCmd(ec),
	)
	f := rootCmd.PersistentFlags()
	f.StringVar(&ec.LogLevel, "log-level", "INFO", "log level (DEBUG, INFO, WARN, ERROR, FATAL)")
	f.StringVar(&ec.ExecutionDirectory, "project", "", "directory where commands are executed. (default: current dir)")
}

// Execute executes the command and returns the error
func Execute() error {
	return rootCmd.Execute()
}
