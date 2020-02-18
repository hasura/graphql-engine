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

func NewActionsCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.GetViper()
	actionsCmd := &cobra.Command{
		Use:   "actions",
		Short: "Manage actions on hasura",
		Example: `  # Create an action
  hasura actions create [action-name]

  # Generate code for an actions
  hasura actions codegen [action-name]

  # Set a framework to be used by codegen
  hasura actions use-codegen`,
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			err = ec.Validate()
			if err != nil {
				return err
			}
			if ec.Config.Version != cli.V2 {
				return fmt.Errorf("actions commands can be executed only when config version is greater than 1")
			}
			if ec.MetadataDir == "" {
				return fmt.Errorf("actions commands can be executed only when metadata_dir is set in config")
			}
			return nil
		},
	}
	actionsCmd.AddCommand(
		newActionsCreateCmd(ec),
		newActionsCodegenCmd(ec),
		newActionsUseCodegenCmd(ec),
	)
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
