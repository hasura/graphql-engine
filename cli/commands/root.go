// Package commands contains the definition for all the commands present in
// Hasura CLI.
package commands

import (
	"fmt"
	"io"
	"os"

	"github.com/hasura/graphql-engine/cli/internal/client"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/update"
	"github.com/hasura/graphql-engine/cli/version"
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
	f.BoolVar(&ec.SkipUpdateCheck, "skip-update-check", false, "Skip automatic update check on command execution")
	f.BoolVar(&ec.NoColor, "no-color", false, "do not colorize output (default: false)")
	f.StringVar(&ec.Envfile, "envfile", ".env", ".env filename to load ENV vars from")
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

func checkIfUpdateToConfigV3IsRequired(ec *cli.ExecutionContext) error {
	// see if an update to config V3 is necessary
	if ec.Config.Version < cli.V3 && ec.HasMetadataV3 {
		// check if the server is setup using a database-url
		hasuraAPIClient, err := client.NewHasuraRestAPIClient(client.NewHasuraRestAPIClientOpts{
			Headers:        ec.HGEHeaders,
			QueryAPIURL:    fmt.Sprintf("%s/%s", ec.Config.Endpoint, "v2/query"),
			MetadataAPIURL: fmt.Sprintf("%s/%s", ec.Config.Endpoint, "v1/metadata"),
			TLSConfig:      ec.Config.TLSConfig,
		})
		if err != nil {
			ec.Logger.Debug(err)
		}
		var hasDefaultDatasource bool
		if yes, err := hasuraAPIClient.HasDefaultDatasource(); yes {
			hasDefaultDatasource = yes
		} else if err != nil {
			ec.Logger.Debug("checking if there is a datasource named default", err)
		}

		hasMultipleDatasources, err := hasuraAPIClient.HasMultipleDatasources()
		if err != nil {
			return err
		}
		if (hasDefaultDatasource && hasMultipleDatasources) || !hasDefaultDatasource {
			// server is configured with a default datasource
			// and other datasources
			ec.Logger.Info("Looks like you are trying to use hasura with multiple datasources, which requires some changes on your project directory\n")
			ec.Logger.Info("please use hasura scripts update-config-v3 to make this change")
			return errors.New("update to config V3")
		}
	}

	return nil
}

// Execute executes the command and returns the error
func Execute() error {
	err := ec.Prepare()
	if err != nil {
		return errors.Wrap(err, "preparing execution context failed")
	}
	execCmd, err := NewDefaultHasuraCommand().ExecuteC()
	if err != nil {
		ec.Telemetry.IsError = true
	}
	ec.Telemetry.Command = execCmd.CommandPath()
	ec.Telemetry.Beam()
	if ec.Spinner != nil {
		ec.Spinner.Stop()
	}
	return err
}
