package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/scripts"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

type rawOutputFormat string

const rawOutputFormatJSON rawOutputFormat = "json"
const rawOutputFormatYAML rawOutputFormat = "yaml"

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

func writeByOutputFormat(w io.Writer, b []byte, format rawOutputFormat) error {
	switch format {
	case rawOutputFormatJSON:
		out := new(bytes.Buffer)
		err := json.Indent(out, b, "", "  ")
		if err != nil {
			return err
		}
		io.Copy(w, out)
	case rawOutputFormatYAML:
		o, err := yaml.JSONToYAML(b)
		if err != nil {
			return err
		}
		io.Copy(w, bytes.NewReader(o))
	default:
		return fmt.Errorf("output format '%v' is not supported. supported formats: %v, %v", format, rawOutputFormatJSON, rawOutputFormatYAML)
	}
	return nil
}

func isJSON(str []byte) bool {
	var js json.RawMessage
	return json.Unmarshal(str, &js) == nil
}

func isYAML(str []byte) bool {
	var y yaml.MapSlice
	return yaml.Unmarshal(str, &y) == nil
}
