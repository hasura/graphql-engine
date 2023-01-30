package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
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
		Short:   "Manage Hasura GraphQL Engine Metadata saved in the database",
		Long: `This command allows you to manage the Hasura GraphQL Engine Metadata saved in the database via a collection of flags and subcommands.
		
Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/index/
`,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PersistentPreRunE")
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return errors.E(op, err)
			}
			err = ec.Validate()
			if err != nil {
				return errors.E(op, err)
			}
			if err := scripts.CheckIfUpdateToConfigV3IsRequired(ec); err != nil {
				return errors.E(op, err)
			}
			return nil
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

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
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
	var op errors.Op = "commands.writeByOutputFormat"
	switch format {
	case rawOutputFormatJSON:
		out := new(bytes.Buffer)
		err := json.Indent(out, b, "", "  ")
		if err != nil {
			return errors.E(op, err)
		}
		_, err = io.Copy(w, out)
		if err != nil {
			return errors.E(op, fmt.Errorf("writing output failed: %w", err))
		}
	case rawOutputFormatYAML:
		o, err := metadatautil.JSONToYAML(b)
		if err != nil {
			return errors.E(op, err)
		}
		_, err = io.Copy(w, bytes.NewReader(o))
		if err != nil {
			return errors.E(op, fmt.Errorf("writing output failed: %w", err))
		}
	default:
		return errors.E(op, fmt.Errorf("output format '%v' is not supported. supported formats: %v, %v", format, rawOutputFormatJSON, rawOutputFormatYAML))
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
