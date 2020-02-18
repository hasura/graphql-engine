package commands

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newActionsCodegenCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &actionsCodegenOptions{
		EC: ec,
	}
	actionsCodegenCmd := &cobra.Command{
		Use:          "codegen [action-name]",
		Short:        "",
		SilenceUsage: true,
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
			if ec.Config.Version == cli.V1 {
				return fmt.Errorf("actions commands can be executed only when config version is greater than 1")
			}
			if ec.MetadataDir == "" {
				return fmt.Errorf("actions commands can be executed only when metadata_dir is set in config")
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.actions = args
			return opts.run()
		},
	}
	f := actionsCodegenCmd.Flags()

	f.StringVar(&opts.deriveFrom, "derive-from", "", "derive action from a Hasura operation")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return actionsCodegenCmd
}

type actionsCodegenOptions struct {
	EC         *cli.ExecutionContext
	actions    []string
	deriveFrom string
}

func (o *actionsCodegenOptions) run() (err error) {
	migrateDrv, err := newMigrate(o.EC, true)
	if err != nil {
		return err
	}

	var derivePayload actions.DerivePayload
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
	if len(o.actions) == 0 {
		actionsFileContent, err := actions.GetActionsFileContent(o.EC.MetadataDir)
		if err != nil {
			return errors.Wrap(err, "error getting actions file content")
		}
		for _, action := range actionsFileContent.Actions {
			codegenActions = append(codegenActions, action.Name)
		}
	} else {
		codegenActions = o.actions
	}

	actionCfg := actions.New(o.EC, o.EC.MetadataDir)
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
