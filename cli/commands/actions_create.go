package commands

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newActionsCreateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &actionsCreateOptions{
		EC: ec,
	}
	actionsCreateCmd := &cobra.Command{
		Use:          "create",
		Short:        "",
		SilenceUsage: true,
		Args:         cobra.ExactArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.name = args[0]
			return opts.run()
		},
	}

	f := actionsCreateCmd.Flags()

	f.StringVar(&opts.deriveFromMutation, "derive-from-mutation", "", "")
	f.StringVar(&opts.scaffolderName, "scaffolder-name", "", "")

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
	scaffolderName     string
}

func (o *actionsCreateOptions) run() error {
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.MetadataDir, o.EC.ServerConfig.Action, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}
	var introSchema interface{}
	if o.deriveFromMutation != "" {
		introSchema, err = migrateDrv.GetIntroSpectionSchema()
		if err != nil {
			return err
		}
	}
	actionCfg := actions.New(o.EC.MetadataDir, o.EC.ServerConfig.Action, o.EC.ServerConfig.Scaffold)
	err = actionCfg.Create(o.name, introSchema, o.deriveFromMutation)
	if err != nil {
		return err
	}
	err = migrateDrv.ApplyMetadata(nil)
	if err != nil {
		return err
	}
	return actionCfg.Scaffold(o.name, o.scaffolderName)
}
