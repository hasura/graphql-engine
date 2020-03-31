package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewMetadataCmd returns the metadata command
func NewMetadataCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	metadataCmd := &cobra.Command{
		Use:     "metadata",
		Aliases: []string{"md"},
		Short:   "Manage Hasura GraphQL Engine metadata saved in the database",
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		SilenceUsage: true,
	}
	metadataCmd.AddCommand(
		newMetadataDiffCmd(ec),
		newMetadataExportCmd(ec),
		newMetadataClearCmd(ec),
		newMetadataReloadCmd(ec),
		newMetadataApplyCmd(ec),
		newMetadataInconsistencyCmd(ec),
	)

	metadataCmd.PersistentFlags().String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	metadataCmd.PersistentFlags().String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	metadataCmd.PersistentFlags().String("access-key", "", "access key for Hasura GraphQL Engine")
	metadataCmd.PersistentFlags().MarkDeprecated("access-key", "use --admin-secret instead")

	v.BindPFlag("endpoint", metadataCmd.PersistentFlags().Lookup("endpoint"))
	v.BindPFlag("admin_secret", metadataCmd.PersistentFlags().Lookup("admin-secret"))
	v.BindPFlag("access_key", metadataCmd.PersistentFlags().Lookup("access-key"))

	return metadataCmd
}

func executeMetadata(cmd string, t *migrate.Migrate, ec *cli.ExecutionContext) error {
	switch cmd {
	case "export":
		files, err := t.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot export metadata from server")
		}
		err = t.WriteMetadata(files)
		if err != nil {
			return errors.Wrap(err, "cannot write metadata")
		}
	case "clear":
		err := t.ResetMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot clear Metadata")
		}
	case "reload":
		err := t.ReloadMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot reload Metadata")
		}
	case "apply":
		err := t.ApplyMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot apply metadata on the database")
		}
		return nil
	}
	return nil
}
