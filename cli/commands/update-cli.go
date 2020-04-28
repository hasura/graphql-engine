package commands

import (
	"fmt"
	"os"
	"strings"

	"github.com/Masterminds/semver"

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
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run(false)
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
	currentVersion := o.EC.Version.CLISemver
	if currentVersion == nil {
		return errors.Errorf("cannot update from a non-semver version: %s", o.EC.Version.GetCLIVersion())
	}

	var versionToBeInstalled *semver.Version
	if o.version != "" {
		// parse the version
		versionToBeInstalled, err = semver.NewVersion(o.version)
		if err != nil {
			return errors.Wrap(err, "unable to parse version")
		}
	} else {
		o.EC.Spin("Checking for update... ")
		hasUpdate, latestVersion, hasPreReleaseUpdate, preReleaseVersion, err := update.HasUpdate(o.EC.HTTPClient, currentVersion, o.EC.LastUpdateCheckFile)
		o.EC.Spinner.Stop()
		if err != nil {
			return errors.Wrap(err, "command: check update")
		}

		ec.Logger.Debugln("hasUpdate: ", hasUpdate, "latestVersion: ", latestVersion, "hasPreReleaseUpdate: ", hasPreReleaseUpdate, "preReleaseVersion: ", preReleaseVersion, "currentVersion:", currentVersion)

		if showPrompt {
			switch {
			case hasUpdate:
				ok := ask2confirm(latestVersion.String(), o.EC.Logger)
				if !ok {
					o.EC.Logger.Info("skipping update, run 'hasura update-cli' to update manually")
					return nil
				}
				versionToBeInstalled = latestVersion
			case hasPreReleaseUpdate:
				o.EC.Logger.Infof(`a new pre-release version is available:
- %s (changelog: %s)
to update cli to this version, execute:

  hasura update-cli --version %s

`, preReleaseVersion.Original(), getChangeLogLink(preReleaseVersion), preReleaseVersion.Original())
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
	err = update.ApplyUpdate(o.EC.HTTPClient, versionToBeInstalled)
	o.EC.Spinner.Stop()
	if err != nil {
		if os.IsPermission(err) {
			return errors.New("permission denied, try again as admin or with sudo")
		}
		return errors.Wrap(err, "apply update")
	}

	o.EC.Logger.WithField("version", "v"+versionToBeInstalled.String()).Info("Updated to latest version")
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

func getChangeLogLink(version *semver.Version) string {
	return fmt.Sprintf("https://github.com/hasura/graphql-engine/releases/tag/%s", version.Original())
}
