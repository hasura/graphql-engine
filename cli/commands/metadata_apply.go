package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataApplyOptions{
		EC: ec,
	}

	metadataApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply Hasura metadata on a database",
		Example: `  # Apply Hasura GraphQL engine metadata present in metadata.[yaml|json] file:
  hasura metadata apply

  # Use with admin secret:
  hasura metadata apply --admin-secret "<admin-secret>"

  # Apply metadata to an instance specified by the flag:
  hasura metadata apply --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			if opts.FromFile {
				return fmt.Errorf("use of deprecated flag")
			}
			if !opts.DryRun && len(opts.rawOutput) == 0 {
				ec.Spin("Applying metadata...")
			}
			err := opts.Run()
			ec.Spinner.Stop()
			if err != nil {
				return err
			}
			if len(opts.rawOutput) <= 0 && !opts.DryRun {
				opts.EC.Logger.Info("Metadata applied")
			}
			return nil
		},
	}

	f := metadataApplyCmd.Flags()

	// deprecated flag
	f.BoolVar(&opts.FromFile, "from-file", false, "apply metadata from migrations/metadata.[yaml|json]")
	if err := f.MarkDeprecated("from-file", "deprecation is a side effect of config v1 deprecation from v2.0.0"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	f.BoolVar(&opts.DryRun, "dry-run", false, "show metadata generated from project directory without applying to server.  generated metadata will be printed as JSON by default, use -o flag for other display formats")
	f.StringVarP(&opts.rawOutput, "output", "o", "", `specify an output format to show applied metadata. Allowed values: json, yaml (default "json")`)
	return metadataApplyCmd
}

type MetadataApplyOptions struct {
	EC *cli.ExecutionContext

	FromFile  bool
	DryRun    bool
	rawOutput string
}

func (o *MetadataApplyOptions) Run() error {
	return getMetadataModeHandler(o.EC.MetadataMode).Apply(o)
}

func errorApplyingMetadata(err error) error {
	// a helper function to have consistent error messages for errors
	// when applying metadata
	return fmt.Errorf("error applying metadata \n%w", err)
}
