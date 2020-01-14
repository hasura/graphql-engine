package commands

import (
	"fmt"
	"io/ioutil"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/plugins/gitutil"
	"github.com/hasura/graphql-engine/cli/plugins/index"
	"github.com/hasura/graphql-engine/cli/plugins/installation"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newScriptsUpdateConfigV2Cmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	var metadataDir string
	scriptsUpdateConfigV2Cmd := &cobra.Command{
		Use:          "update-config-v2",
		Short:        "",
		Example:      ``,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if ec.Config.Version != "1" {
				return fmt.Errorf("this script can be executed only when the current version is 1")
			}
			// update the plugin index
			err := gitutil.EnsureCloned(ec.PluginsPath.IndexPath())
			if err != nil {
				return errors.Wrap(err, "cannot update plugin index")
			}
			// update current config to v2
			ec.Config.Version = "2"
			ec.Config.MetadataDirectory = metadataDir
			ec.Config.Action = actions.ActionExecutionConfig{
				Kind:                  ec.Viper.GetString("actions.kind"),
				HandlerWebhookBaseURL: ec.Viper.GetString("actions.handler_webhook_baseurl"),
			}
			// save the config.yaml file
			data, err := yaml.Marshal(ec.Config)
			if err != nil {
				return errors.Wrap(err, "cannot convert to yaml")
			}
			err = ioutil.WriteFile(ec.ConfigFile, data, 0644)
			if err != nil {
				return errors.Wrap(err, "cannot write config file")
			}
			// install the plugin
			plugin, err := index.LoadPluginByName(ec.PluginsPath.IndexPluginsPath(), "cli-ext")
			if err != nil {
				return errors.Wrap(err, "cannot load plugin manifest")
			}
			err = installation.Install(ec.PluginsPath, plugin)
			if err != nil && err != installation.ErrIsAlreadyInstalled {
				return errors.Wrap(err, "cannot install plugin")
			}
			// reload the config
			err = ec.Validate()
			if err != nil {
				return errors.Wrap(err, "cannot validate new config")
			}
			// run metadata export
			migrateDrv, err := newMigrate(ec, true)
			if err != nil {
				return errors.Wrap(err, "unable to initialize migrations driver")
			}
			err = migrateDrv.ExportMetadata()
			if err != nil {
				return errors.Wrap(err, "cannot export metadata")
			}
			return nil
		},
	}

	f := scriptsUpdateConfigV2Cmd.Flags()
	f.StringVar(&metadataDir, "metadata-dir", "metadata", "")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return scriptsUpdateConfigV2Cmd
}
