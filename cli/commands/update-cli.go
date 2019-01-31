package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/update"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

// NewUpdateCLICmd returns the update-cli command.
func NewUpdateCLICmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &updateOptions{
		EC: ec,
	}
	updateCmd := &cobra.Command{
		Use:          "update-cli",
		Short:        "Update the CLI to latest version",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}
	return updateCmd
}

type updateOptions struct {
	EC *cli.ExecutionContext
}

func (o *updateOptions) run() error {
	currentVersion := o.EC.Version.CLISemver
	if currentVersion == nil {
		return errors.Errorf("cannot update from a non-semver version: %s", o.EC.Version.GetCLIVersion())
	}

	o.EC.Spin("Checking for update... ")
	hasUpdate, latestVersion, err := update.HasUpdate(currentVersion)
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.Wrap(err, "command: check update")
	}

	if !hasUpdate {
		o.EC.Logger.WithField("version", currentVersion).Info("hasura cli is up to date")
		return nil
	}

	o.EC.Spin(fmt.Sprintf("Updating cli to %s... ", latestVersion.String()))
	err = update.ApplyUpdate(latestVersion)
	o.EC.Spinner.Stop()
	if err != nil {
		if os.IsPermission(err) {
			return errors.New("permission denied, try again as admin or with sudo")
		}
		return errors.Wrap(err, "apply update")
	}

	o.EC.Logger.WithField("version", latestVersion.String()).Info("Updated to latest version")
	return nil
}
