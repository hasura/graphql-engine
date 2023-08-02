package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/spf13/cobra"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataApplyOptions{
		EC: ec,
	}

	metadataApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply Hasura Metadata on a database",
		Long: `This command applies the Hasura GraphQL Engine Metadata saved in the database. You can use it to apply Hasura Metadata from one HGE server instance to another, such as when moving between development environments.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-metadata/
- https://hasura.io/docs/latest/migrations-metadata-seeds/metadata-format/
`,
		Example: `  # Apply Hasura GraphQL Engine metadata present in metadata.[yaml|json] file:
  hasura metadata apply

  # Use with admin secret:
  hasura metadata apply --admin-secret "<admin-secret>"

  # Apply metadata to an instance specified by the flag:
  hasura metadata apply --endpoint "<endpoint>"
  
  # Prevent inconsistent metadata from getting applied:
  hasura metadata apply --disallow-inconsistent-metadata`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if opts.FromFile {
				return errors.E(op, fmt.Errorf("use of deprecated flag"))
			}
			if !opts.DryRun && len(opts.rawOutput) == 0 {
				ec.Spin("Applying metadata...")
			}
			err := opts.Run()
			ec.Spinner.Stop()
			if err != nil {
				return errors.E(op, err)
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
	f.BoolVar(&opts.DisallowInconsistencies, "disallow-inconsistent-metadata", false, "disallow inconsistent metadata to be applied. Defaults to false")
	return metadataApplyCmd
}

type MetadataApplyOptions struct {
	EC *cli.ExecutionContext

	FromFile                bool
	DryRun                  bool
	rawOutput               string
	DisallowInconsistencies bool
}

func (o *MetadataApplyOptions) Run() error {
	var op errors.Op = "commands.MetadataApplyOptions.Run"
	err := getMetadataModeHandler(o.EC.MetadataMode).Apply(o)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func errorApplyingMetadata(err error) error {
	var op errors.Op = "commands.errorApplyingMetadata"
	// a helper function to have consistent error messages for errors
	// when applying metadata
	return errors.E(op, fmt.Errorf("error applying metadata \n%w", err))
}
