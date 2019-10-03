// Package commands contains the definition for all the commands present in
// Hasura CLI.
package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/update"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const hasuraASCIIText = `
    __
   / /_   ____ _ _____ __  __ _____ ____ _
  / __ \ / __ ` + "`" + `// ___// / / // ___// __ ` + "`" + `/
 / / / // /_/ /(__  )/ /_/ // /   / /_/ /
/_/ /_/ \__,_//____/ \__,_//_/    \__,_/

`

// ec is the Execution Context for the current run.
var ec *cli.ExecutionContext

// rootCmd is the main "hasura" command
var rootCmd = &cobra.Command{
	Use:           "hasura",
	Short:         "Hasura GraphQL Engine command line tool",
	Long:          hasuraASCIIText,
	SilenceUsage:  true,
	SilenceErrors: true,
	PersistentPreRun: func(cmd *cobra.Command, args []string) {
		ec.Telemetry.Command = cmd.CommandPath()

		if cmd.Use != updateCLICmdUse {
			if update.ShouldRunCheck(ec.LastUpdateCheckFile) && ec.GlobalConfig.ShowUpdateNotification && !ec.SkipUpdateCheck {
				u := &updateOptions{
					EC: ec,
				}
				err := u.run(true)
				if err != nil {
					ec.Logger.WithError(err).Error("auto-update failed, run 'hasura update-cli' to update manually")
				}
			}
		}
	},
	Run: func(cmd *cobra.Command, args []string) {
		o := helpOptions{
			EC:   ec,
			Cmd:  cmd,
			Args: args,
		}
		o.run()
	},
}

func init() {
	ec = cli.NewExecutionContext()
	rootCmd.AddCommand(
		NewInitCmd(ec),
		NewConsoleCmd(ec),
		NewMetadataCmd(ec),
		NewMigrateCmd(ec),
		NewVersionCmd(ec),
		NewDocsCmd(ec),
		NewCompletionCmd(ec),
		NewUpdateCLICmd(ec),
	)
	rootCmd.SetHelpCommand(NewHelpCmd(ec))
	f := rootCmd.PersistentFlags()
	f.StringVar(&ec.LogLevel, "log-level", "INFO", "log level (DEBUG, INFO, WARN, ERROR, FATAL)")
	f.StringVar(&ec.ExecutionDirectory, "project", "", "directory where commands are executed (default: current dir)")
	f.BoolVar(&ec.SkipUpdateCheck, "skip-update-check", false, "Skip automatic update check on command execution")
	f.BoolVar(&ec.NoColor, "no-color", false, "do not colorize output (default: false)")
}

// Execute executes the command and returns the error
func Execute() error {
	err := ec.Prepare()
	if err != nil {
		return errors.Wrap(err, "preparing execution context failed")
	}
	err = rootCmd.Execute()
	if err != nil {
		ec.Telemetry.IsError = true
	}
	ec.Telemetry.Beam()
	if ec.Spinner != nil {
		ec.Spinner.Stop()
	}
	return err
}
