package commands

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/hasura/graphql-engine/cli/plugins/gitutil"
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
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if ec.Config.Version != "1" {
				return fmt.Errorf("this script can be executed only when the current config version is 1")
			}
			// update the plugin index
			ec.Spin("Updating the plugin index...")
			defer ec.Spinner.Stop()
			err := gitutil.EnsureUpdated(ec.Plugins.Paths.IndexPath())
			if err != nil {
				return errors.Wrap(err, "cannot update plugin index")
			}
			// install the plugin
			ec.Spin("Installing cli-ext plugin...")
			err = ec.Plugins.Install("cli-ext", "")
			if err != nil && err != plugins.ErrIsAlreadyInstalled {
				return errors.Wrap(err, "cannot install plugin")
			}
			// update current config to v2
			ec.Spin("Updating current config to 2")
			os.Setenv("HASURA_GRAPHQL_VERSION", "2")
			os.Setenv("HASURA_GRAPHQL_METADATA_DIRECTORY", metadataDir)
			os.Setenv("HASURA_GRAPHQL_ACTION_KIND", ec.Viper.GetString("actions.kind"))
			os.Setenv("HASURA_GRAPHQL_ACTION_HANDLER_WEBHOOK_BASEURL", ec.Viper.GetString("actions.handler_webhook_baseurl"))
			defer func() {
				// unset env
				os.Unsetenv("HASURA_GRAPHQL_VERSION")
				os.Unsetenv("HASURA_GRAPHQL_METADATA_DIRECTORY")
				os.Unsetenv("HASURA_GRAPHQL_ACTION_KIND")
				os.Unsetenv("HASURA_GRAPHQL_ACTION_HANDLER_WEBHOOK_BASEURL")
			}()
			ec.Spin("Reloading config file...")
			err = ec.Validate()
			if err != nil {
				return errors.Wrap(err, "cannot validate new config")
			}
			defer func() {
				if err != nil {
					os.RemoveAll(ec.MetadataDir)
				}
			}()
			// set codegen to nil, so that it is not exported in yaml
			ec.Config.Action.Codegen = nil
			// run metadata export
			ec.Spin("Exporting metadata...")
			migrateDrv, err := newMigrate(ec, true)
			if err != nil {
				return errors.Wrap(err, "unable to initialize migrations driver")
			}
			err = migrateDrv.ExportMetadata()
			if err != nil {
				return errors.Wrap(err, "cannot export metadata")
			}
			ec.Spin("Writing new config file...")
			data, err := yaml.Marshal(ec.Config)
			if err != nil {
				return errors.Wrap(err, "cannot convert to yaml")
			}
			err = ioutil.WriteFile(ec.ConfigFile, data, 0644)
			if err != nil {
				return errors.Wrap(err, "cannot write config file")
			}
			ec.Spinner.Stop()
			ec.Logger.Infoln("Updated config to version 2")
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
