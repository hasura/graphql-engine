package commands

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/migrate"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func newActionsCodegenCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &actionsCodegenOptions{
		EC: ec,
	}
	actionsCodegenCmd := &cobra.Command{
		Use:   "codegen [action-name]",
		Short: "Generate code for actions",
		Example: `  # Generate code for all actions
  hasura actions codegen

  # Generate code for an action
  hasura actions codegen [action-name]

  # Generate code for two or more actions
  hasura actions codegen [action-name] [action-name...]

  # Derive an action from a hasura operation
  hasura actions codegen [action-name] --derive-from ""`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.actions = args
			return opts.run()
		},
	}
	f := actionsCodegenCmd.Flags()

	f.StringVar(&opts.deriveFrom, "derive-from", "", "derive action from a hasura operation")
	return actionsCodegenCmd
}

type actionsCodegenOptions struct {
	EC         *cli.ExecutionContext
	actions    []string
	deriveFrom string
}

func (o *actionsCodegenOptions) run() (err error) {
	migrateDrv, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return err
	}

	var derivePayload types.DerivePayload
	if o.deriveFrom != "" {
		derivePayload.Operation = strings.TrimSpace(o.deriveFrom)
		o.EC.Spin("Deriving a Hasura operation...")
		introSchema, err := migrateDrv.GetIntroSpectionSchema()
		if err != nil {
			return errors.Wrap(err, "unable to fetch introspection schema")
		}
		derivePayload.IntrospectionSchema = introSchema
		o.EC.Spinner.Stop()
	}

	if o.EC.Config.ActionConfig.Codegen.Framework == "" {
		return fmt.Errorf(`Could not find codegen config. For adding codegen config, run:

  hasura actions use-codegen`)
	}

	// if no actions are passed, perform codegen for all actions
	o.EC.Spin("Generating code...")
	var codegenActions []string
	actionCfg := actions.New(o.EC, o.EC.MetadataDir)
	if len(o.actions) == 0 {
		actionsFileContent, err := actionCfg.GetActionsFileContent()
		if err != nil {
			return errors.Wrap(err, "error getting actions file content")
		}
		for _, action := range actionsFileContent.Actions {
			codegenActions = append(codegenActions, action.Name)
		}
	} else {
		codegenActions = o.actions
	}

	for _, actionName := range codegenActions {
		err = actionCfg.Codegen(actionName, derivePayload)
		if err != nil {
			return errors.Wrapf(err, "error generating codegen for action %s", actionName)
		}
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen files generated at " + o.EC.Config.ActionConfig.Codegen.OutputDir)
	return nil
}
