package commands

import (
	"bytes"
	"fmt"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/spf13/cobra"
)

func newMigrateStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MigrateStatusOptions{
		EC:         ec,
		StatusOpts: make(StatusOptions),
	}
	migrateStatusCmd := &cobra.Command{
		Use:   "status",
		Short: "Display current status of migrations on a database",
		Long:  "When running this command, the CLI will allow you to select which database - or all - to run the status command on. The CLI will return the current status of migrations on the selected database(s), including the version, name, source, database, and status.",
		Example: `  # Use with admin secret:
  hasura migrate status --admin-secret "<your-admin-secret>"

  # Check status on a different server:
  hasura migrate status --endpoint "<endpoint>"`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			var op = genOpName(cmd, "PreRunE")
			if err := validateConfigV3FlagsWithAll(cmd, ec); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			var op = genOpName(cmd, "RunE")
			if err := opts.Run(); err != nil {
				return errors.E(op, err)
			}
			buf := printStatus(opts.StatusOpts)
			fmt.Fprintf(ec.Stdout, "%s", buf)
			return nil
		},
	}
	return migrateStatusCmd
}

func (o *MigrateStatusOptions) Run() error {
	var op errors.Op = "commands.MigrateStatusOptions.Run"
	o.EC.Spin("Fetching migration status...")
	defer o.EC.Spinner.Stop()
	if ec.AllDatabases {
		sourcesAndKind, err := metadatautil.GetSourcesAndKind(o.EC.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			return errors.E(op, fmt.Errorf("got error while getting the sources list : %v", err))
		}
		for _, source := range sourcesAndKind {
			o.Source = cli.Source(source)
			status, err := o.RunOnSource()
			if err != nil {
				return errors.E(op, fmt.Errorf("error getting status for database '%s': %v", o.Source.Name, err))
			}
			o.StatusOpts[o.Source] = status
		}
		return nil
	}
	o.Source = ec.Source
	status, err := o.RunOnSource()
	if err != nil {
		return errors.E(op, fmt.Errorf("error getting status for database '%s': %v", o.Source.Name, err))
	}
	o.StatusOpts[o.Source] = status
	return nil
}

type StatusOptions map[cli.Source]*migrate.Status
type MigrateStatusOptions struct {
	EC         *cli.ExecutionContext
	Source     cli.Source
	StatusOpts StatusOptions
}

func (o *MigrateStatusOptions) RunOnSource() (*migrate.Status, error) {
	var op errors.Op = "commands.MigrateStatusOptions.RunOnSource"
	if o.EC.Config.Version <= cli.V2 {
		o.Source.Name = ""
		o.Source.Kind = hasura.SourceKindPG
	}
	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return nil, errors.E(op, err)
	}
	status, err := executeStatus(migrateDrv)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("cannot fetch migrate status: %w", err))
	}
	return status, nil
}

func printStatus(statusOpts StatusOptions) *bytes.Buffer {
	out := new(tabwriter.Writer)
	buf := &bytes.Buffer{}
	out.Init(buf, 0, 8, 2, ' ', 0)
	w := util.NewPrefixWriter(out)
	for source, status := range statusOpts {
		if source.Name != "" {
			w.Write(util.LEVEL_0, fmt.Sprintf("\nDatabase: %s\n", source.Name))
		}
		w.Write(util.LEVEL_0, "VERSION\tNAME\tSOURCE STATUS\tDATABASE STATUS\n")
		for _, version := range status.Index {
			w.Write(util.LEVEL_0, "%d\t%s\t%s\t%s\n",
				version,
				status.Migrations[version].Name,
				convertBool(status.Migrations[version].IsPresent),
				convertBool(status.Migrations[version].IsApplied),
			)
		}
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
