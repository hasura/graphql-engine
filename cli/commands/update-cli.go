package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/update"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

// NewUpdateCmd checks and update to lastest graphql-engine cli
func NewUpdateCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &updateOptions{
		EC: ec,
	}
	updateCmd := &cobra.Command{
		Use:          "update-cli",
		Short:        "Update the Hasura CLI to latest version",
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
	currentVersion := o.EC.Version.GetCLIVersion()
	o.EC.Logger.Infof("Current version: %s", currentVersion)
	o.EC.Spin("Checking for update... ")
	hasUpdate, releaseInfo, asset, err := update.CheckUpdate(currentVersion)
	o.EC.Spinner.Stop()
	if err != nil {
		return err
	}

	if !hasUpdate {
		o.EC.Logger.WithField("version", currentVersion).Info("CLI is up to date")
		return nil
	}

	o.EC.Spin(fmt.Sprintf("Updating to %s... ", releaseInfo.TagName))
	err = update.ApplyUpdate(asset)
	o.EC.Spinner.Stop()
	if err != nil {
		if os.IsPermission(err) {
			return errors.New("permission denied, try again as admin or with sudo")
		}
		return errors.Wrap(err, "apply update")
	}

	o.EC.Logger.WithField("version", releaseInfo.TagName).Info("Updated to latest version")
	return nil
}
