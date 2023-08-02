// Package commands contains the definition for all the commands present in
// Hasura CLI.
package commands

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/update"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
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
		if cmd.Use != updateCLICmdUse {
			if update.ShouldRunCheck(ec.LastUpdateCheckFile) && ec.GlobalConfig.ShowUpdateNotification && !ec.SkipUpdateCheck {
				u := &updateOptions{
					EC: ec,
				}
				err := u.run(true)
				if err != nil && u.EC.Version.GetCLIVersion() != version.DevVersion {
					ec.Logger.WithError(err).Warn("auto-update failed, run 'hasura update-cli' to update manually")
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
		NewSeedCmd(ec),
		NewDeployCmd(ec),
		NewActionsCmd(ec),
		NewPluginsCmd(ec),
		NewVersionCmd(ec),
		NewScriptsCmd(ec),
		NewDocsCmd(ec),
		NewCompletionCmd(ec),
		NewUpdateCLICmd(ec),
	)
	rootCmd.SetHelpCommand(NewHelpCmd(ec))
	f := rootCmd.PersistentFlags()
	f.StringVar(&ec.LogLevel, "log-level", "INFO", "log level (DEBUG, INFO, WARN, ERROR, FATAL)")
	f.StringVar(&ec.ExecutionDirectory, "project", "", "directory where commands are executed (default: current dir)")
	f.BoolVar(&ec.SkipUpdateCheck, "skip-update-check", false, "skip automatic update check on command execution")
	f.BoolVar(&ec.NoColor, "no-color", false, "do not colorize output (default: false)")
	f.StringVar(&ec.Envfile, "envfile", ".env", ".env filename to load ENV vars from")
	f.StringVar(&ec.CliExtSourceBinPath, "cli-ext-path", "", "path to cli-ext binary")
	if err := f.MarkHidden("cli-ext-path"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
}

// NewDefaultHasuraCommand creates the `hasura` command with default arguments
func NewDefaultHasuraCommand() *cobra.Command {
	return NewDefaultHasuraCommandWithArgs(NewDefaultPluginHandler(validPluginFilenamePrefixes), os.Args, os.Stdin, os.Stdout, os.Stderr)
}

// NewDefaultHasuraCommandWithArgs creates the `hasura` command with arguments
func NewDefaultHasuraCommandWithArgs(pluginHandler PluginHandler, args []string, in io.Reader, out, errout io.Writer) *cobra.Command {
	cmd := rootCmd

	if pluginHandler == nil {
		return cmd
	}

	if len(args) > 1 {
		cmdPathPieces := args[1:]

		// only look for suitable extension executables if
		// the specified command does not already exist
		if _, _, err := cmd.Find(cmdPathPieces); err != nil {
			if err := HandlePluginCommand(pluginHandler, cmdPathPieces); err != nil {
				fmt.Fprintf(errout, "%v\n", err)
				os.Exit(1)
			}
		}
	}

	return cmd
}

// Execute executes the command and returns the error
func Execute() error {
	var op errors.Op = "commands.Execute"
	err := ec.Prepare()
	if err != nil {
		return errors.E(op, fmt.Errorf("preparing execution context failed: %w", err))
	}
	execCmd, err := NewDefaultHasuraCommand().ExecuteC()
	if err != nil {
		ec.Telemetry.IsError = true
		ec.Telemetry.Error = err
	}
	commandPath := execCmd.CommandPath()
	command := []string{commandPath}
	getFlagName := func(f *pflag.Flag) {
		flagName := fmt.Sprintf("--%s", f.Name)
		command = append(command, flagName)
	}
	execCmd.Flags().Visit(getFlagName)
	ec.Telemetry.Command = strings.Join(command, " ")
	ec.Telemetry.Beam()
	if ec.Spinner != nil {
		ec.Spinner.Stop()
	}
	if err != nil {
		if e, ok := err.(*errors.Error); ok {
			ec.Logger.WithFields(logrus.Fields{
				"ops":      errors.Ops(e),
				"kind":     errors.GetKind(e),
				"location": errors.GetLocation(e).String(),
			}).Debug(err)
		}
		return errors.E(op, err)
	}
	return nil
}

func genOpName(cmd *cobra.Command, funcName string) errors.Op {
	return errors.Op("command: " + cmd.CommandPath() + "." + funcName)
}
