package commands

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewActionsCmd returns the actions command
func NewActionsCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	actionsCmd := &cobra.Command{
		Use:          "actions",
		Short:        "Manage actions on hasura",
		SilenceUsage: true,
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
			if ec.Config.Version < cli.V2 {
				return fmt.Errorf("actions commands can be executed only when config version is greater than 1")
			}
			if ec.MetadataDir == "" {
				return fmt.Errorf("actions commands can be executed only when metadata_dir is set in config")
			}
			return nil
		},
	}

	actionsCmd.AddCommand(
		newActionsCreateCmd(ec, v),
		newActionsCodegenCmd(ec),
		newActionsUseCodegenCmd(ec),
	)

	actionsCmd.PersistentFlags().String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	actionsCmd.PersistentFlags().String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	actionsCmd.PersistentFlags().String("access-key", "", "access key for Hasura GraphQL Engine")
	actionsCmd.PersistentFlags().MarkDeprecated("access-key", "use --admin-secret instead")

	v.BindPFlag("endpoint", actionsCmd.PersistentFlags().Lookup("endpoint"))
	v.BindPFlag("admin_secret", actionsCmd.PersistentFlags().Lookup("admin-secret"))
	v.BindPFlag("access_key", actionsCmd.PersistentFlags().Lookup("access-key"))

	return actionsCmd
}

func getCodegenFrameworks() (allFrameworks []codegenFramework, err error) {
	frameworkFileBytes, err := ioutil.ReadFile(filepath.Join(ec.GlobalConfigDir, util.ActionsCodegenDirName, "frameworks.json"))
	if err != nil {
		return
	}
	err = json.Unmarshal(frameworkFileBytes, &allFrameworks)
	if err != nil {
		return
	}
	return
}
