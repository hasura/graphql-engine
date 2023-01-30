package commands

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newActionsCreateCmd(ec *cli.ExecutionContext, v *viper.Viper) *cobra.Command {
	opts := &actionsCreateOptions{
		EC: ec,
	}
	actionsCreateCmd := &cobra.Command{
		Use:   "create [action-name]",
		Short: "Create a Hasura Action",
		Long: `This command allows you to create an Action to extend Hasura's schema with custom business logic using queries and mutations. Optional flags can be used to derive the Action from an existing GraphQL query or mutation. Additionally, codegen can be bundled with the creation of the Action to provide you ready-to-use boilerplate with your framework of choice.
		
Further Reading:
- https://hasura.io/docs/latest/actions/create/
`,
		Example: `  # Create a Hasura Action
  hasura actions create [action-name]

  # Create a Hasura Action with codegen
  hasura actions create [action-name] --with-codegen

  # Create a Hasura Action by deriving from a Hasura operation
  hasura actions create [action-name] --derive-from ''

  # Create a Hasura Action with a different kind or webhook
  hasura actions create [action-name] --kind [synchronous|asynchronous] --webhook [http://localhost:3000]`,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.name = args[0]
			if err := opts.run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	f := actionsCreateCmd.Flags()

	f.StringVar(&opts.deriveFrom, "derive-from", "", "derive action from a Hasura operation")
	f.BoolVar(&opts.withCodegen, "with-codegen", false, "create Action along with codegen")
	f.String("kind", "", "kind to use in Action")
	f.String("webhook", "", "webhook to use in Action")

	// bind to viper
	util.BindPFlag(v, "actions.kind", f.Lookup("kind"))
	util.BindPFlag(v, "actions.handler_webhook_baseurl", f.Lookup("webhook"))

	return actionsCreateCmd
}

type actionsCreateOptions struct {
	EC *cli.ExecutionContext

	name        string
	deriveFrom  string
	withCodegen bool
}

func (o *actionsCreateOptions) run() error {
	var op errors.Op = "commands.actionsCreateOptions.run"
	var introSchema hasura.IntrospectionSchema
	var err error
	if o.deriveFrom != "" {
		o.deriveFrom = strings.TrimSpace(o.deriveFrom)
		o.EC.Spin("Deriving a Hasura operation...")
		introSchema, err = o.EC.APIClient.V1Graphql.GetIntrospectionSchema()
		if err != nil {
			return errors.E(op, fmt.Errorf("error in fetching introspection schema: %w", err))
		}
		o.EC.Spinner.Stop()
	}

	// create new action
	o.EC.Spin("Creating the action...")
	actionCfg := actions.New(o.EC, o.EC.MetadataDir)
	o.EC.Spinner.Stop()
	err = actionCfg.Create(o.name, introSchema, o.deriveFrom)
	if err != nil {
		return errors.E(op, fmt.Errorf("error in creating action: %w", err))
	}
	opts := &MetadataApplyOptions{
		EC: o.EC,
	}
	err = opts.Run()
	if err != nil {
		return errors.E(op, fmt.Errorf("error in applying metadata: %w", err))
	}
	o.EC.Logger.WithField("name", o.name).Infoln("action created")

	// if codegen config not present, skip codegen
	if o.EC.Config.ActionConfig.Codegen.Framework == "" {
		if o.withCodegen {
			return errors.E(op, fmt.Errorf(`could not find codegen config. For adding codegen config, run:

  hasura actions use-codegen`))
		}
		return nil
	}

	// if with-codegen flag not present, ask them if they want to codegen
	var confirmation bool
	if !o.withCodegen {
		confirmation, err = util.GetYesNoPrompt("Do you want to generate " + o.EC.Config.ActionConfig.Codegen.Framework + " code for this action and the custom types?")
		if err != nil {
			return errors.E(op, fmt.Errorf("error in getting user input: %w", err))
		}
	}

	if !confirmation {
		infoMsg := fmt.Sprintf(`You skipped codegen. For getting codegen for this action, run:

  hasura actions codegen %s
`, o.name)
		o.EC.Logger.Info(infoMsg)
		return nil
	}

	if err := o.EC.SetupCodegenAssetsRepo(); err != nil {
		o.EC.Logger.Errorf("failed generating code: setting up codegen-assets repo failed (this is required for automatically generating actions code): %v", err)
		o.EC.Logger.Errorf("retry operation with: 'hasura actions codegen %s'", o.name)
		return nil
	}
	// ensure codegen-assets repo exists
	if err := ec.CodegenAssetsRepo.EnsureCloned(); err != nil {
		o.EC.Logger.Errorf("failed generating code: pulling latest actions codegen files from internet failed: %v", err)
		o.EC.Logger.Errorf("retry operation with: 'hasura actions codegen %s'", o.name)
		return nil
	}

	// construct derive payload to send to codegenerator
	derivePayload := types.DerivePayload{
		IntrospectionSchema: introSchema,
		Operation:           o.deriveFrom,
		ActionName:          o.name,
	}

	// Run codegen
	o.EC.Spin(fmt.Sprintf(`Running "hasura actions codegen %s"...`, o.name))
	err = actionCfg.Codegen(o.name, derivePayload)
	if err != nil {
		o.EC.Spinner.Stop()
		o.EC.Logger.Warn("codegen failed, retry with `hasura actions codegen`")
		return errors.E(op, err)
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen files generated at " + o.EC.Config.ActionConfig.Codegen.OutputDir)
	return nil

}
