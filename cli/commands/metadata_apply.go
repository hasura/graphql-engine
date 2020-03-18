package commands

import (
	"os"

	"github.com/hasura/graphql-engine/cli/migrate"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
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

	f.BoolVar(&opts.FromFile, "from-file", false, "apply metadata from migrations/metadata.[yaml|json]")
	f.BoolVar(&opts.dryRun, "dry-run", false, "show a diff instead of applying the metadata")

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

	migrateDrv, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return err
	}
	return executeMetadata(o.ActionType, migrateDrv, o.EC)
}
