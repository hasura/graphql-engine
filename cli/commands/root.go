// Package commands contains the definition for all the commands present in
// Hasura CLI.
package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/telemetry"
	"github.com/hasura/graphql-engine/cli/version"
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
		EC.Telemetry = telemetry.BuildEvent()
		EC.Telemetry.Version = version.BuildVersion
		EC.Telemetry.Command = cmd.CommandPath()

		err := EC.Prepare()
		if err != nil {
			EC.Logger.Debugf("EC.Prepare failed: %v", err)
			return errors.Wrap(err, "initializing the command failed")
		}
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
