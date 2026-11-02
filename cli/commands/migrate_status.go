package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"sort"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/hasura/graphql-engine/cli/v2/util"
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
  hasura migrate status --endpoint "<endpoint>"

  # Print the status as JSON, e.g. for scripts:
  hasura migrate status --output json`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")

			err := validateConfigV3FlagsWithAll(cmd, ec)
			if err != nil {
				return errors.E(op, err)
			}

			if opts.OutputFormat != "" && opts.OutputFormat != "table" && opts.OutputFormat != "json" {
				return errors.E(op, fmt.Errorf("invalid output format %q: allowed values are table and json", opts.OutputFormat))
			}

			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")

			err := opts.Run()
			if err != nil {
				return errors.E(op, err)
			}

			if opts.OutputFormat == "json" {
				out, err := statusToJSON(opts.StatusOpts)
				if err != nil {
					return errors.E(op, err)
				}

				fmt.Fprintln(ec.Stdout, string(out))

				return nil
			}

			buf := printStatus(opts.StatusOpts)
			fmt.Fprintf(ec.Stdout, "%s", buf)

			return nil
		},
	}

	f := migrateStatusCmd.Flags()
	f.StringVarP(&opts.OutputFormat, "output", "o", "", "output format (Allowed values: table, json)")

	return migrateStatusCmd
}

func (o *MigrateStatusOptions) Run() error {
	var op errors.Op = "commands.MigrateStatusOptions.Run"

	o.EC.Spin("Fetching migration status...")
	defer o.EC.Spinner.Stop()

	if ec.AllDatabases {
		sourcesAndKind, err := metadatautil.GetSourcesAndKind(
			o.EC.APIClient.V1Metadata.ExportMetadata,
		)
		if err != nil {
			return errors.E(op, fmt.Errorf("got error while getting the sources list : %w", err))
		}

		for _, source := range sourcesAndKind {
			o.Source = cli.Source(source)

			status, err := o.RunOnSource()
			if err != nil {
				return errors.E(
					op,
					fmt.Errorf("error getting status for database '%s': %w", o.Source.Name, err),
				)
			}

			o.StatusOpts[o.Source] = status
		}

		return nil
	}

	o.Source = ec.Source

	status, err := o.RunOnSource()
	if err != nil {
		return errors.E(
			op,
			fmt.Errorf("error getting status for database '%s': %w", o.Source.Name, err),
		)
	}

	o.StatusOpts[o.Source] = status

	return nil
}

type (
	StatusOptions        map[cli.Source]*migrate.Status
	MigrateStatusOptions struct {
		EC           *cli.ExecutionContext
		Source       cli.Source
		StatusOpts   StatusOptions
		OutputFormat string
	}
)

type migrationStatusJSON struct {
	Version uint64 `json:"version"`
	Name    string `json:"name"`
	// PresentInSource and AppliedOnDatabase are the SOURCE STATUS and
	// DATABASE STATUS columns of the table output
	PresentInSource   bool `json:"present_in_source"`
	AppliedOnDatabase bool `json:"applied_on_database"`
}

type databaseStatusJSON struct {
	Database   string                `json:"database"`
	Migrations []migrationStatusJSON `json:"migrations"`
}

// statusToJSON renders the status of every database as a JSON array, sorted
// by database name, with the migrations in version order.
func statusToJSON(statusOpts StatusOptions) ([]byte, error) {
	databases := make([]databaseStatusJSON, 0, len(statusOpts))

	for source, status := range statusOpts {
		migrations := make([]migrationStatusJSON, 0, len(status.Index))
		for _, version := range status.Index {
			m := status.Migrations[version]
			migrations = append(migrations, migrationStatusJSON{
				Version:           version,
				Name:              m.Name,
				PresentInSource:   m.IsPresent,
				AppliedOnDatabase: m.IsApplied,
			})
		}

		databases = append(databases, databaseStatusJSON{Database: source.Name, Migrations: migrations})
	}

	sort.Slice(databases, func(i, j int) bool { return databases[i].Database < databases[j].Database })

	return json.MarshalIndent(databases, "", "  ")
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
