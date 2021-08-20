package commands

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
	"github.com/spf13/viper"
)

func NewDeployCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &DeployOptions{
		EC: ec,
		MetadataApplyOpts: MetadataApplyOptions{
			EC: ec,
		},
		MigrateApplyOpts: MigrateApplyOptions{
			EC:           ec,
			AllDatabases: true,
		},
		MetadataReloadOpts: MetadataReloadOptions{
			EC: ec,
		},
	}
	deployCmd := &cobra.Command{
		Use:   "deploy",
		Short: "Apply all server metadata & database migrations",
		Example: `  # Apply Hasura GraphQL engine metadata present in metadata.[yaml|json] file,
  # run all migrations for all the databases and reload metadata:
  hasura deploy

  # Use with admin secret:
  hasura metadata apply --admin-secret "<admin-secret>"

  # Apply metadata to an instance specified by the flag:
  hasura metadata apply --endpoint "<endpoint>"`,
		SilenceUsage: false,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			if err := ec.Validate(); err != nil {
				return err
			}
			return nil
		},
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return validateConfigV3Flags(cmd, ec)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			if err := ec.Prepare(); err != nil {
				return err
			}
			if err := ec.Validate(); err != nil {
				return err
			}
			if err := opts.Run(); err != nil {
				return err
			}
			return nil
		},
	}

	flags := deployCmd.Flags()
	flags.SortFlags = false

	flagConfig := &pflag.Flag{Name: "all-databases", Changed: true, Hidden: true}
	flags.AddFlag(flagConfig)

	migrateCommonFlags(flags, v)
	flags.Set("disable-interactive", "true")
	flags.MarkHidden("disable-interactive")
	flags.MarkHidden("database-name")
	return deployCmd
}

type DeployOptions struct {
	EC                 *cli.ExecutionContext
	MetadataApplyOpts  MetadataApplyOptions
	MigrateApplyOpts   MigrateApplyOptions
	MetadataReloadOpts MetadataReloadOptions
}

func (opts *DeployOptions) Run() error {
	if err := opts.MetadataApplyOpts.Run(); err != nil {
		return err
	}
	if err := opts.MigrateApplyOpts.Run(); err != nil {
		return err
	}
	if err := opts.MetadataReloadOpts.runWithInfo(); err != nil {
		return err
	}
	return nil
}
