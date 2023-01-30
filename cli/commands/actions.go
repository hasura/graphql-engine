package commands

import (
	"encoding/json"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewActionsCmd returns the actions command
func NewActionsCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	actionsCmd := &cobra.Command{
		Use:   "actions",
		Short: "Manage Hasura Actions",
		Long: `Running this command enables the use of additional sub-commands to create, modify, and export code related to a project's Actions.

Further Reading:
- https://hasura.io/docs/latest/actions/index/
- https://hasura.io/docs/latest/actions/create/
- https://hasura.io/docs/latest/actions/derive/
`,
		SilenceUsage: true,
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
			if ec.Config.Version < cli.V2 {
				return errors.E(op, "actions commands can be executed only when config version is greater than 1")
			}

			if ec.MetadataDir == "" {
				return errors.E(op, "actions commands can be executed only when metadata_dir is set in config")
			}
			return nil
		},
	}

	actionsCmd.AddCommand(
		newActionsCreateCmd(ec, v),
		newActionsCodegenCmd(ec),
		newActionsUseCodegenCmd(ec),
	)

	f := actionsCmd.PersistentFlags()

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

	return actionsCmd
}

func getCodegenFrameworks() (allFrameworks []codegenFramework, err error) {
	var op errors.Op = "commands.getCodegenFrameworks"
	frameworkFileBytes, err := ioutil.ReadFile(filepath.Join(ec.GlobalConfigDir, util.ActionsCodegenDirName, "frameworks.json"))
	if err != nil {
		return allFrameworks, errors.E(op, err)
	}
	err = json.Unmarshal(frameworkFileBytes, &allFrameworks)
	if err != nil {
		return allFrameworks, errors.E(op, err)
	}
	return allFrameworks, nil
}
