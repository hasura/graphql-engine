package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/internal/scripts"
	"github.com/hasura/graphql-engine/cli/util"
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
		Short:   "Manage Hasura GraphQL engine metadata saved in the database",
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			err = ec.Validate()
			if err != nil {
				return err
			}
			return scripts.CheckIfUpdateToConfigV3IsRequired(ec)
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

	f := metadataCmd.PersistentFlags()

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL engine")
	f.String("access-key", "", "access key for Hasura GraphQL engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")
	f.Bool("insecure-skip-tls-verify", false, "skip TLS verification and disable cert checking (default: false)")
	f.String("certificate-authority", "", "path to a cert file for the certificate authority")

	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))

	return metadataCmd
}

func executeMetadata(cmd string, ec *cli.ExecutionContext) error {
	metadataobject.SetMetadataObjectsWithDir(ec, ec.MetadataDir)
	var files map[string][]byte
	var err error
	metadataHandler := metadataobject.NewHandlerFromEC(ec)
	switch cmd {
	case "export":
		files, err = metadataHandler.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot export metadata from server")
		}
		err = metadataHandler.WriteMetadata(files)
		if err != nil {
			return errors.Wrap(err, "cannot write metadata")
		}
	case "clear":
		err = metadataHandler.ResetMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot clear Metadata")
		}
	case "reload":
		err = metadataHandler.ReloadMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot reload Metadata")
		}
	case "apply":
		if ec.Config.Version <= cli.V2 {
			err := metadataHandler.V1ApplyMetadata()
			if err != nil {
				return errors.Wrap(err, "cannot apply metadata on the database")
			}
			return nil
		}
		if err := metadataHandler.V2ApplyMetadata(); err != nil {
			return fmt.Errorf("\n%w", err)
		}
	}
	return nil
}
