package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataApplyOptions{
		EC:         ec,
		actionType: "apply",
	}

	metadataApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply Hasura metadata on a database",
		Example: `  # Apply Hasura GraphQL Engine metadata present in metadata.[yaml|json] file:
  hasura metadata apply`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if opts.dryRun {
				o := &metadataDiffOptions{
					EC:     ec,
					output: os.Stdout,
				}
				m := metadata.New(o.EC.MigrationDir)
				filename, err := m.GetExistingMetadataFile()
				if err != nil {
					return errors.Wrap(err, "failed getting metadata file")
				}
				o.metadata[0] = filename
				return o.run()
			}
			opts.EC.Spin("Applying metadata...")
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to apply metadata")
			}
			opts.EC.Logger.Info("Metadata applied")
			return nil
		},
	}

	f := metadataApplyCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	f.BoolVar(&opts.fromFile, "from-file", false, "apply metadata from migrations/metadata.[yaml|json]")
	f.BoolVar(&opts.dryRun, "dry-run", false, "show a diff instead of applying the metadata")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataApplyCmd
}

type metadataApplyOptions struct {
	EC *cli.ExecutionContext

	actionType string

	fromFile bool
	dryRun   bool
}

func (o *metadataApplyOptions) run() error {
	if o.fromFile {
		o.EC.MetadataDir = ""
	}
	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	return executeMetadata(o.actionType, migrateDrv, o.EC)
}
