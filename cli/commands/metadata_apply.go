package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &MetadataApplyOptions{
		EC:         ec,
		ActionType: "apply",
	}

	metadataApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply Hasura metadata on a database",
		Example: `  # Apply Hasura GraphQL Engine metadata present in metadata.[yaml|json] file:
  hasura metadata apply

  # Use with admin secret:
  hasura metadata apply --admin-secret "<admin-secret>"

  # Apply metadata to an instance specified by the flag:
  hasura metadata apply --endpoint "<endpoint>"`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if opts.dryRun {
				o := &MetadataDiffOptions{
					EC:     ec,
					Output: os.Stdout,
					Args:   []string{},
				}
				return o.Run()
			}
			opts.EC.Spin("Applying metadata...")
			err := opts.Run()
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

	f.BoolVar(&opts.FromFile, "from-file", false, "apply metadata from migrations/metadata.[yaml|json]")
	f.BoolVar(&opts.dryRun, "dry-run", false, "show a diff instead of applying the metadata")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataApplyCmd
}

type MetadataApplyOptions struct {
	EC *cli.ExecutionContext

	ActionType string

	FromFile bool
	dryRun   bool
}

func (o *MetadataApplyOptions) Run() error {
	if o.FromFile {
		actualMetadataDir := o.EC.MetadataDir
		o.EC.MetadataDir = ""
		defer func() {
			o.EC.MetadataDir = actualMetadataDir
		}()
	}

	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}
	return executeMetadata(o.ActionType, migrateDrv, o.EC)
}
