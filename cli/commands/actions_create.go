package commands

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newActionsCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &actionsCreateOptions{
		EC: ec,
	}
	actionsCreateCmd := &cobra.Command{
		Use:               "create",
		Short:             "",
		SilenceUsage:      true,
		Args:              cobra.ExactArgs(1),
		PersistentPreRunE: ensureCLIExtension,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			err = ec.Validate()
			if err != nil {
				return err
			}
			if ec.Config.Version == "1" {
				return fmt.Errorf("actions commands can be executed only when config version is greater than 1")
			}
			if ec.MetadataDir == "" {
				return fmt.Errorf("actions commands can be executed only when metadata_dir is set in config")
			}
			return ensureCLIExtension(cmd, args)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			return opts.run()
		},
	}

	f := actionsCreateCmd.Flags()

	f.StringVar(&opts.deriveFromMutation, "derive-from-mutation", "", "derive action from a Hasura mutation")
	f.BoolVar(&opts.withCodegen, "with-codegen", false, "create action along with codegen")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return actionsCreateCmd
}

type actionsCreateOptions struct {
	EC *cli.ExecutionContext

	name               string
	deriveFromMutation string
	withCodegen        bool
}

func (o *actionsCreateOptions) run() error {
	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}

	o.name = strings.TrimSpace(o.name)

	// introspect Hasura schema if a mutation is being derived
	var introSchema interface{}
	if o.deriveFromMutation != "" {
		o.deriveFromMutation = strings.TrimSpace(o.deriveFromMutation)
		o.EC.Spin("Deriving a Hasura mutation...")
		introSchema, err = migrateDrv.GetIntroSpectionSchema()
		if err != nil {
			return err
		}
	}
	o.EC.Spinner.Stop()

	// create new action
	actionCfg := actions.New(o.EC)
	err = actionCfg.Create(o.name, introSchema, o.deriveFromMutation)
	if err != nil {
		return err
	}
	o.EC.Spin("Creating the action...")
	err = migrateDrv.ApplyMetadata()
	if err != nil {
		return err
	}

	o.EC.Spinner.Stop()
	o.EC.Logger.WithField("name", o.name).Infoln("action created")

	// if codegen config not present, skip codegen
	if o.EC.Config.Action.Codegen.Framework == "" {
		if o.withCodegen {
			infoMsg := fmt.Sprintf(`Could not find codegen config in config.yaml. For setting codegen config, run:

  hasura actions use-codegen`)
			o.EC.Logger.Error(infoMsg)
			return nil
		}
		return nil
	}

	// if with-codegen flag not present, ask them if they want to codegen
	var confirmation string
	if !o.withCodegen {
		confirmation, err = util.GetYesNoPrompt("Do you want to generate " + o.EC.Config.Action.Codegen.Framework + " code for this action and the custom types?")
		if err != nil {
			return err
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
	derivePayload := actions.DerivePayload{
		IntrospectionSchema: introSchema,
		Mutation: actions.DeriveMutationPayload{
			MutationName: o.deriveFromMutation,
			ActionName:   o.name,
		},
	}

	// Run codegen
	o.EC.Spin(fmt.Sprintf(`Running "hasura actions codegen %s"...`, o.name))
	err = actionCfg.Codegen(o.name, derivePayload)
	if err != nil {
		return err
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen files generated at " + o.EC.Config.Action.Codegen.OutputDir)
	return nil

}
