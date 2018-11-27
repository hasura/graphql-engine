// Package commands contains the definition for all the commands present in
// Hasura CLI.
package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/telemetry"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

// EC is the Execution Context for the current run.
var EC = &cli.ExecutionContext{}

// rootCmd is the main "hasura" command
var rootCmd = &cobra.Command{
	Use:           "hasura",
	Short:         "Hasura GraphQL Engine command line tool",
	SilenceUsage:  true,
	SilenceErrors: true,
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		err := EC.Prepare()
		if err != nil {
			EC.Logger.Debugf("EC.Prepare failed: %v", err)
			return errors.Wrap(err, "initializing the command failed")
		}
		telemetry.Waiter.Add(1)
		go func() {
			defer telemetry.Waiter.Done()
			telemetry.SendExecutionEvent(EC, cmd, args)
		}()
		EC.Spin("Running...")
		return nil
	},
}

func init() {
	rootCmd.AddCommand(
		NewInitCmd(EC),
		NewConsoleCmd(EC),
		NewMetadataCmd(EC),
		NewMigrateCmd(EC),
		NewVersionCmd(EC),
		NewDocsCmd(EC),
	)
	f := rootCmd.PersistentFlags()
	f.StringVar(&EC.LogLevel, "log-level", "INFO", "log level (DEBUG, INFO, WARN, ERROR, FATAL)")
	f.StringVar(&EC.ExecutionDirectory, "project", "", "directory where commands are executed. (default: current dir)")
}

// Execute executes the command and returns the error
func Execute() error {
	return rootCmd.Execute()
}
