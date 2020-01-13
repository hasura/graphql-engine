package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newActionsCodegenCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &actionsCodegenOptions{
		EC: ec,
	}
	actionsCodegenCmd := &cobra.Command{
		Use:               "codegen",
		Short:             "",
		SilenceUsage:      true,
		Args:              cobra.ExactArgs(1),
		PersistentPreRunE: ensureCLIExtension,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Validate()
			if err != nil {
				return err
			}
			if ec.MetadataDir == "" {
				return fmt.Errorf("actions commands can be executed only when metadata_dir is set in config")
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			return opts.run()
		},
	}

	return actionsCodegenCmd
}

type actionsCodegenOptions struct {
	EC   *cli.ExecutionContext
	name string
}

func (o *actionsCodegenOptions) run() error {
	actionCfg := actions.New(o.EC.MetadataDir, o.EC.Config.Action, o.EC.CMDName)
	derivePayload := actions.DerivePayload{}
	return actionCfg.Codegen(o.name, derivePayload)
}
