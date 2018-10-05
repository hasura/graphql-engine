package commands

import (
	"bytes"
	"fmt"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMigrateStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateStatusOptions{
		EC: ec,
	}
	migrateStatusCmd := &cobra.Command{
		Use:          "status",
		Short:        "Display current status of migrations on a database",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			status, err := opts.run()
			if err != nil {
				return err
			}
			buf := PrintStatus(status)
			fmt.Println(buf.String())
			return nil
		},
	}

	f := migrateStatusCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return migrateStatusCmd
}

type migrateStatusOptions struct {
	EC *cli.ExecutionContext
}

func (o *migrateStatusOptions) run() (*migrate.Status, error) {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.Config.ParsedEndpoint, o.EC.Config.AccessKey, o.EC.Logger)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}
	status, err := executeStatus(migrateDrv)
	if err != nil {
		return nil, errors.Wrap(err, "cannot fetch migrate status")
	}
	return status, nil
}

func PrintStatus(status *migrate.Status) *bytes.Buffer {
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
	return buf
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
