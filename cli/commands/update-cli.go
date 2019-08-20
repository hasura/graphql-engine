package commands

import (
	"fmt"
	"os"
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/update"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

const updateCLICmdUse = "update-cli"

const updateCLICmdExample = `  # Update CLI to latest version:
  hasura update-cli

  # To disable auto-update check on the CLI, set
  # "show_update_notification": false
  # in ~/.hasura/config.json
`

// NewUpdateCLICmd returns the update-cli command.
func NewUpdateCLICmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &updateOptions{
		EC: ec,
	}
	updateCmd := &cobra.Command{
		Use:          updateCLICmdUse,
		Short:        "Update the CLI to latest version",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run(false)
		},
		Example: updateCLICmdExample,
	}
	return updateCmd
}

type updateOptions struct {
	EC *cli.ExecutionContext
}

func (o *updateOptions) run(showPrompt bool) error {
	currentVersion := o.EC.Version.CLISemver
	if currentVersion == nil {
		return errors.Errorf("cannot update from a non-semver version: %s", o.EC.Version.GetCLIVersion())
	}

	o.EC.Spin("Checking for update... ")
	hasUpdate, latestVersion, err := update.HasUpdate(currentVersion, o.EC.LastUpdateCheckFile)
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.Wrap(err, "command: check update")
	}

	if !hasUpdate {
		o.EC.Logger.WithField("version", currentVersion).Info("hasura cli is up to date")
		return nil
	}

	if showPrompt {
		ok := ask2confirm(latestVersion.String(), o.EC.Logger)
		if !ok {
			o.EC.Logger.Info("skipping update, run 'hasura update-cli' to update manually")
			return nil
		}
	}

	o.EC.Spin(fmt.Sprintf("Updating cli to v%s... ", latestVersion.String()))
	err = update.ApplyUpdate(latestVersion)
	o.EC.Spinner.Stop()
	if err != nil {
		if os.IsPermission(err) {
			return errors.New("permission denied, try again as admin or with sudo")
		}
		return errors.Wrap(err, "apply update")
	}

	o.EC.Logger.WithField("version", "v"+latestVersion.String()).Info("Updated to latest version")
	return nil
}

func ask2confirm(v string, log *logrus.Logger) bool {
	var s string

	log.Infof("A new version (v%s) is available for CLI, update? (y/N)", v)
	_, err := fmt.Scan(&s)
	if err != nil {
		log.Error("unable to take input, skipping update")
		return false
	}

	s = strings.TrimSpace(s)
	s = strings.ToLower(s)

	if s == "y" || s == "yes" {
		return true
	}
	return false
}
