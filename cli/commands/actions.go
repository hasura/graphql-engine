package commands

import (
	"encoding/json"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/spf13/cobra"
)

func NewActionsCmd(ec *cli.ExecutionContext) *cobra.Command {
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
