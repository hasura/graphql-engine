package commands

import (
  "fmt"
	"io/ioutil"

  "github.com/hasura/graphql-engine/cli"
  "github.com/spf13/cobra"
  "github.com/spf13/viper"
	"github.com/ghodss/yaml"
)

func newActionsUseCodegenCmd(ec *cli.ExecutionContext) *cobra.Command {
  v := viper.New()
  opts := &actionsUseCodegenOptions{
    EC: ec,
  }
  actionsCreateCmd := &cobra.Command{
    Use:               "use-codegen",
    Short:             "",
    SilenceUsage:      true,
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
      return opts.run()
    },
  }

  f := actionsCreateCmd.Flags()

  f.StringVar(&opts.framework, "framework", "", "")
  f.StringVar(&opts.outputDir, "output-dir", "", "")
  f.BoolVar(&opts.withStarterKit, "with-starter-kit", false, "")

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

type actionsUseCodegenOptions struct {
  EC *cli.ExecutionContext

  framework string
  outputDir  string
  withStarterKit bool
}

func (o *actionsUseCodegenOptions) run() (err error) {
	if o.framework == "" {
		fmt.Println("Should ask interactive")
	}

  o.EC.Config.Action.Codegen.Framework = o.framework

  if o.outputDir != "" {
  	o.EC.Config.Action.Codegen.OutputDir = o.outputDir
  }
 
  configString, err := yaml.Marshal(o.EC.Config)
  if err != nil {
  	return
  }

	err = ioutil.WriteFile(o.EC.ConfigFile, configString, 0644)
	if err != nil {
		return
	}

	return err
}
