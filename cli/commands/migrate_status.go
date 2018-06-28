package commands

import (
	"net/url"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func NewMigrateStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &migrateStatusOptions{
		EC: ec,
	}
	migrateStatusCmd := &cobra.Command{
		Use:          "status",
		Short:        "Display current status of migrations on a database",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	return migrateStatusCmd
}

type migrateStatusOptions struct {
	EC *cli.ExecutionContext
}

func (o *migrateStatusOptions) run() error {
	dbURL, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "error parsing Endpoint")
	}

	dbURL.Scheme = "hasuradb"
	dbURL.User = url.UserPassword("admin", o.EC.Config.AccessKey)
	return util.ExecuteStatus("file://"+o.EC.MigrationDir, dbURL.String())
}
