package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/update"
	"github.com/spf13/cobra"
)

// NewUpdateCmd checks and update to lastest graphql-engine cli
func NewUpdateCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &updateOptions{
		EC: ec,
	}
	updateCmd := &cobra.Command{
		Use:          "update-cli",
		Short:        "Update graphql-engine CLI tool to latest version",
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
	isUpdate, releaseInfo, asset, err := update.CheckUpdate(o.EC.Version.GetCLIVersion())
	if err != nil {
		return err
	}

	if !isUpdate {
		o.EC.Logger.WithField("version", o.EC.Version.GetCLIVersion()).Info("hasura is up to date")
		return nil
	}

	err = update.ApplyUpdate(asset)
	if err != nil {
		if os.IsPermission(err) {
			o.EC.Logger.Fatal(`# permission denied, try in admin mode or sudo:
     $ sudo hasura update-cli`)
			return nil
		}
		return err
	}

	o.EC.Logger.WithField("version", releaseInfo.TagName).Info("hasura updated to latest version")
	return nil
}
