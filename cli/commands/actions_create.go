package commands

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/actions"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newActionsCreateCmd(ec *cli.ExecutionContext, v *viper.Viper) *cobra.Command {
	opts := &actionsCreateOptions{
		EC: ec,
	}
	actionsCreateCmd := &cobra.Command{
		Use:   "create [action-name]",
		Short: "Create a Hasura action",
		Example: `  # Create a Hasura action
  hasura actions create [action-name]

  # Create a Hasura action with codegen
  hasura actions create [action-name] --with-codegen

  # Create a Hasura action by deriving from a hasura operation
  hasura actions create [action-name] --derive-from ''

  # Create a Hasura action with a different kind or webhook
  hasura actions create [action-name] --kind [synchronous|asynchronous] --webhook [http://localhost:3000]`,
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			return opts.run()
		},
	}

	f := actionsCreateCmd.Flags()

	f.StringVar(&opts.deriveFrom, "derive-from", "", "derive action from a Hasura operation")
	f.BoolVar(&opts.withCodegen, "with-codegen", false, "create action along with codegen")
	f.String("kind", "", "kind to use in action")
	f.String("webhook", "", "webhook to use in action")

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
	var introSchema hasura.IntrospectionSchema
	var err error
	if o.deriveFrom != "" {
		o.deriveFrom = strings.TrimSpace(o.deriveFrom)
		o.EC.Spin("Deriving a Hasura operation...")
		introSchema, err = o.EC.APIClient.V1Graphql.GetIntrospectionSchema()
		if err != nil {
			return errors.Wrap(err, "error in fetching introspection schema")
		}
		o.EC.Spinner.Stop()
	}

	// create new action
	o.EC.Spin("Creating the action...")
	actionCfg := actions.New(o.EC, o.EC.MetadataDir)
	o.EC.Spinner.Stop()
	err = actionCfg.Create(o.name, introSchema, o.deriveFrom)
	if err != nil {
		return errors.Wrap(err, "error in creating action")
	}
	err = executeMetadata("apply", o.EC)
	if err != nil {
		return errors.Wrap(err, "error in applying metadata")
	}
	o.EC.Logger.WithField("name", o.name).Infoln("action created")

	// if codegen config not present, skip codegen
	if o.EC.Config.ActionConfig.Codegen.Framework == "" {
		if o.withCodegen {
			return fmt.Errorf(`Could not find codegen config. For adding codegen config, run:

  hasura actions use-codegen`)
		}
		return nil
	}

	// if with-codegen flag not present, ask them if they want to codegen
	var confirmation string
	if !o.withCodegen {
		confirmation, err = util.GetYesNoPrompt("Do you want to generate " + o.EC.Config.ActionConfig.Codegen.Framework + " code for this action and the custom types?")
		if err != nil {
			return errors.Wrap(err, "error in getting user input")
		}
	}

	if confirmation == "n" {
		infoMsg := fmt.Sprintf(`You skipped codegen. For getting codegen for this action, run:

  hasura actions codegen %s
`, o.name)
		o.EC.Logger.Info(infoMsg)
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
		return errors.Wrap(err, "error in generating codegen")
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen files generated at " + o.EC.Config.ActionConfig.Codegen.OutputDir)
	return nil

}
