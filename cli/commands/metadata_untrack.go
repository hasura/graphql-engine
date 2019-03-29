package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataUnTrackCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataUnTrackOptions{
		EC: ec,
	}

	metadataUnTrackCmd := &cobra.Command{
		Use:          "untrack",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := metadataUnTrackCmd.Flags()
	f.BoolVar(&opts.allTables, "all-tables", false, "untrack all tables in the given schemas")
	f.BoolVar(&opts.allRelationships, "all-relationships", false, "untrack all relationships across the given tables")
	f.StringSliceVar(&opts.schemas, "schema", []string{"public"}, "untrack tables from this schema")
	f.StringSliceVar(&opts.tables, "table", []string{}, "untrack these tables in the given schema")
	f.BoolVar(&opts.exportAsMigration, "export-as-migration", false, "export the changes as a migration file")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataUnTrackCmd
}

type metadataUnTrackOptions struct {
	EC *cli.ExecutionContext

	allTables         bool
	allRelationships  bool
	schemas           []string
	tables            []string
	exportAsMigration bool
}

func (o *metadataUnTrackOptions) run() error {
	if o.allTables && len(o.tables) != 0 {
		return errors.New("can't set both --all-tables && --table flag")
	}

	config, err := metadata.NewDefaultConfig(o.EC.ServerConfig.Endpoint, o.EC.ServerConfig.AdminSecret)
	if err != nil {
		return errors.Wrap(err, "can't create new metadata config")
	}

	for _, schema := range o.schemas {
		config.AppendSchema(schema)
	}
	for _, table := range o.tables {
		config.AppendTable(table)
	}
	config.AllTables = o.allTables
	config.AllRelationShips = o.allRelationships
	if o.exportAsMigration {
		timestamp := getTime()
		createOptions := mig.New(timestamp, "untrack", o.EC.MigrationDir)
		config.ExportAsMigration = createOptions
	}

	err = config.Scan()
	if err != nil {
		return errors.Wrap(err, "can't scan the database")
	}

	err = config.UnTrack()
	if err != nil {
		return errors.Wrap(err, "can't execute untrack query")
	}
	return nil
}
