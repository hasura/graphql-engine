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

	f := actionsCmd.PersistentFlags()

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))

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
