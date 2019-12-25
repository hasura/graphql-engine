package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataInconsistencyStatusCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataInconsistencyListOptions{
		EC: ec,
	}

	metadataInconsistencyStatusCmd := &cobra.Command{
		Use:          "status",
		Short:        "Check if the metadata is inconsistent or not",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.EC.Spin("reading metadata status...")
			err := opts.read()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to read metadata status")
			}
			if opts.isConsistent {
				opts.EC.Logger.Println("metadata is consistent")
			} else {
				return errors.New("metadata is inconsistent, use list command to see the objects")
			}
			return nil
		},
	}

	f := metadataInconsistencyStatusCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataInconsistencyStatusCmd
}
