package commands

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"

	"github.com/spf13/cobra"
)

func newActionsCodegenCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &actionsCodegenOptions{
		EC: ec,
	}
	actionsCodegenCmd := &cobra.Command{
		Use:   "codegen [action-name]",
		Short: "Generate code for Actions",
		Long: `Running this command will generate code for either specified or all Actions. The CLI allows you to select a framework and language for generating code. Further, you also have the option of cloning a starter kit of your chosen framework and choosing the output directory for the generated code.

Further Reading:
- https://hasura.io/docs/latest/actions/codegen/index/
`,
		Example: `  # Generate code for all Actions
  hasura actions codegen

  # Generate code for an Action
  hasura actions codegen [action-name]

  # Generate code for two or more Actions
  hasura actions codegen [action-name] [action-name...]

  # Derive an Action from a Hasura operation
  hasura actions codegen [action-name] --derive-from ""`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if err := ec.SetupCodegenAssetsRepo(); err != nil {
				return errors.E(op, fmt.Errorf("setting up codegen-assets repo failed (this is required for automatically generating actions code): %w", err))
			}
			// ensure codegen-assets repo exists
			if err := ec.CodegenAssetsRepo.EnsureCloned(); err != nil {
				return errors.E(op, fmt.Errorf("pulling latest actions codegen files from internet failed: %w", err))
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.actions = args
			if err := opts.run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}
	f := actionsCodegenCmd.Flags()

	f.StringVar(&opts.deriveFrom, "derive-from", "", "derive Action from a Hasura operation")
	return actionsCodegenCmd
}

type actionsCodegenOptions struct {
	EC         *cli.ExecutionContext
	actions    []string
	deriveFrom string
}

func (o *actionsCodegenOptions) run() (err error) {
	var op errors.Op = "commands.actionsCodegenOptions.run"
	var derivePayload types.DerivePayload
	if o.deriveFrom != "" {
		derivePayload.Operation = strings.TrimSpace(o.deriveFrom)
		o.EC.Spin("Deriving a Hasura operation...")
		introSchema, err := o.EC.APIClient.V1Graphql.GetIntrospectionSchema()
		if err != nil {
			return errors.E(op, fmt.Errorf("unable to fetch introspection schema: %w", err))
		}
		derivePayload.IntrospectionSchema = introSchema
		o.EC.Spinner.Stop()
	}

	if o.EC.Config.ActionConfig.Codegen.Framework == "" {
		return errors.E(op, fmt.Errorf(`could not find codegen config. For adding codegen config, run:

  hasura actions use-codegen`))
	}

	// if no actions are passed, perform codegen for all actions
	o.EC.Spin("Generating code...")
	var codegenActions []string
	actionCfg := actions.New(o.EC, o.EC.MetadataDir)
	if len(o.actions) == 0 {
		actionsFileContent, err := actionCfg.GetActionsFileContent()
		if err != nil {
			return errors.E(op, fmt.Errorf("error getting actions file content: %w", err))
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
			return errors.E(op, fmt.Errorf("error generating codegen for action %s: %w", actionName, err))
		}
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen files generated at " + o.EC.Config.ActionConfig.Codegen.OutputDir)
	return nil
}
