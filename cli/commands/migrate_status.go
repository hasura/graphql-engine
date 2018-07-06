package commands

import (
	"bytes"
	"fmt"
	"net/url"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMigrateStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &migrateStatusOptions{
		EC: ec,
	}
	migrateStatusCmd := &cobra.Command{
		Use:          "status",
		Short:        "Display current status of migrations on a database",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			status, err := opts.run()
			if err != nil {
				return err
			}
			printStatus(status)
			return nil
		},
	}

	return migrateStatusCmd
}

type migrateStatusOptions struct {
	EC *cli.ExecutionContext
}

func (o *migrateStatusOptions) run() (*migrate.Status, error) {
	dbURL, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return nil, errors.Wrap(err, "error parsing Endpoint")
	}

	dbURL.Scheme = "hasuradb"
	dbURL.User = url.UserPassword("admin", o.EC.Config.AccessKey)
	status, err := util.ExecuteStatus("file://"+o.EC.MigrationDir, dbURL.String())
	if err != nil {
		return nil, errors.Wrap(err, "cannot fetch migrate status")
	}
	return status, nil
}

func printStatus(status *migrate.Status) {
	out := new(tabwriter.Writer)
	buf := &bytes.Buffer{}
	out.Init(buf, 0, 8, 2, ' ', 0)
	w := util.NewPrefixWriter(out)
	w.Write(util.LEVEL_0, "VERSION\tSOURCE STATUS\tDATABASE STATUS\n")
	for _, version := range status.Index {
		w.Write(util.LEVEL_0, "%d\t%s\t%s\n",
			version,
			convertBool(status.Migrations[version].IsPresent),
			convertBool(status.Migrations[version].IsApplied),
		)
	}
	out.Flush()
	fmt.Println(buf.String())
}

func convertBool(ok bool) string {
	switch ok {
	case true:
		return "Present"
	case false:
		return "Not Present"
	}
	return ""
}
