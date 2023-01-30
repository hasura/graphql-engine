package commands

import (
	"fmt"
	"os"

	"github.com/Masterminds/semver"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/update"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

const updateCLICmdUse = "update-cli"

const updateCLICmdExample = `  # Update CLI to latest version:
  hasura update-cli

  # To disable auto-update check on the CLI, set
  # "show_update_notification": false
  # in ~/.hasura/config.json

  # Update CLI to a specific version (say v1.2.0-beta.1):
  hasura update-cli --version v1.2.0-beta.1
`

// NewUpdateCLICmd returns the update-cli command.
func NewUpdateCLICmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &updateOptions{
		EC: ec,
	}
	updateCmd := &cobra.Command{
		Use:          updateCLICmdUse,
		Short:        "Update the CLI to latest or a specific version",
		Long:         "You can use this command to update the CLI to the latest version or a specific version. Each time you run a CLI command, if a new version is available, you will be prompted to update the CLI.",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if err := ec.Prepare(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if err := opts.run(false); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		Example: updateCLICmdExample,
	}

	f := updateCmd.Flags()
	f.StringVar(&opts.version, "version", "", "a specific version to install")

	return updateCmd
}

type updateOptions struct {
	EC *cli.ExecutionContext

	version string
}

func (o *updateOptions) run(showPrompt bool) (err error) {
	var op errors.Op = "commands.updateOptions.run"
	currentVersion := o.EC.Version.CLISemver
	if currentVersion == nil {
		return errors.E(op, fmt.Errorf("cannot update from a non-semver version: %s", o.EC.Version.GetCLIVersion()))
	}

	var versionToBeInstalled *semver.Version
	if o.version != "" {
		// parse the version
		versionToBeInstalled, err = semver.NewVersion(o.version)
		if err != nil {
			return errors.E(op, fmt.Errorf("unable to parse version: %w", err))
		}
	} else {
		o.EC.Spin("Checking for update... ")
		hasUpdate, latestVersion, hasPreReleaseUpdate, preReleaseVersion, err := update.HasUpdate(currentVersion, o.EC.LastUpdateCheckFile)
		o.EC.Spinner.Stop()
		if err != nil {
			return errors.E(op, fmt.Errorf("command: check update: %w", err))
		}

		ec.Logger.Debugln("hasUpdate: ", hasUpdate, "latestVersion: ", latestVersion, "hasPreReleaseUpdate: ", hasPreReleaseUpdate, "preReleaseVersion: ", preReleaseVersion, "currentVersion:", currentVersion)

		if showPrompt {
			switch {
			case hasUpdate:
                o.EC.Logger.Infof("A new version (v%s) is available for CLI, you can update it by running 'hasura update-cli'", latestVersion.String())
                return nil
			case hasPreReleaseUpdate:
				o.EC.Logger.WithFields(logrus.Fields{
					"version":   preReleaseVersion.Original(),
					"changelog": getChangeLogLink(preReleaseVersion),
				}).Infof(`a new pre-release version is available`)
				o.EC.Logger.Infof(`to update cli to this version, execute:

  hasura update-cli --version %s

`, preReleaseVersion.Original())
				return nil
			}
		} else {
			if hasUpdate {
				versionToBeInstalled = latestVersion
			}
		}
	}

	if versionToBeInstalled == nil {
		o.EC.Logger.WithField("version", currentVersion).Info("hasura cli is up to date")
		return nil
	}

	ec.Logger.Debugln("versionToBeInstalled: ", versionToBeInstalled.String())

	o.EC.Spin(fmt.Sprintf("Updating cli to v%s... ", versionToBeInstalled.String()))
	err = update.ApplyUpdate(versionToBeInstalled)
	o.EC.Spinner.Stop()
	if err != nil {
		if os.IsPermission(err) {
			return errors.E(op, "permission denied, try again as admin or with sudo")
		}
		return errors.E(op, fmt.Errorf("apply update: %w", err))
	}

	o.EC.Logger.WithField("version", "v"+versionToBeInstalled.String()).Info("Updated to latest version")
	return nil
}

func getChangeLogLink(version *semver.Version) string {
	return fmt.Sprintf("https://github.com/hasura/graphql-engine/releases/tag/%s", version.Original())
}
