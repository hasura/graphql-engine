package commands

import (
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	"github.com/hasura/graphql-engine/cli/metadata/allowlist"
	"github.com/hasura/graphql-engine/cli/metadata/functions"
	"github.com/hasura/graphql-engine/cli/metadata/querycollections"
	"github.com/hasura/graphql-engine/cli/metadata/remoteschemas"
	"github.com/hasura/graphql-engine/cli/metadata/tables"
	metadataTypes "github.com/hasura/graphql-engine/cli/metadata/types"
	metadataVersion "github.com/hasura/graphql-engine/cli/metadata/version"
	"github.com/hasura/graphql-engine/cli/util"

	"github.com/hasura/graphql-engine/cli"
	"github.com/manifoldco/promptui"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const (
	defaultDirectory string = "hasura"
)

// NewInitCmd is the definition for init command
func NewInitCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &InitOptions{
		EC: ec,
	}
	initCmd := &cobra.Command{
		Use:   "init [directory-name]",
		Short: "Initialize directory for Hasura GraphQL Engine migrations",
		Long:  "Create directories and files required for enabling migrations on Hasura GraphQL Engine",
		Example: `  # Create a directory to store migrations
  hasura init [directory-name]

  # Now, edit <my-directory>/config.yaml to add endpoint and admin secret

  # Create a directory with endpoint and admin secret configured:
  hasura init <my-project> --endpoint https://my-graphql-engine.com --admin-secret adminsecretkey

  # Create a hasura project in the current working directory
  hasura init .

  # See https://hasura.io/docs/1.0/graphql/manual/migrations/index.html for more details`,
		SilenceUsage: true,
		Args:         cobra.MaximumNArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = viper.New()
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.PluginsConfig.Repo.EnsureCloned()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(args) == 1 {
				opts.InitDir = args[0]
			}
			return opts.Run()
		},
	}

	f := initCmd.Flags()
	f.Var(cli.NewConfigVersionValue(cli.V2, &opts.Version), "version", "config version to be used")
	f.StringVar(&opts.InitDir, "directory", "", "name of directory where files will be created")
	f.StringVar(&opts.Endpoint, "endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "access-key", "", "access key for Hasura GraphQL Engine")
	f.StringVar(&opts.Template, "install-manifest", "", "install manifest to be cloned")
	f.MarkDeprecated("access-key", "use --admin-secret instead")
	f.MarkDeprecated("directory", "use directory-name argument instead")

	return initCmd
}

type InitOptions struct {
	EC *cli.ExecutionContext

	Version     cli.ConfigVersion
	Endpoint    string
	AdminSecret string
	InitDir     string

	Template string
}

func (o *InitOptions) Run() error {
	var infoMsg string
	// prompt for init directory if it's not set already
	if o.InitDir == "" {
		p := promptui.Prompt{
			Label:   "Name of project directory ",
			Default: defaultDirectory,
		}
		r, err := p.Run()
		if err != nil {
			return handlePromptError(err)
		}
		if strings.TrimSpace(r) != "" {
			o.InitDir = r
		} else {
			o.InitDir = defaultDirectory
		}
	}

	cwdir, err := os.Getwd()
	if err != nil {
		return errors.Wrap(err, "error getting current working directory")
	}
	initPath, err := filepath.Abs(o.InitDir)
	if err != nil {
		return err
	}
	if initPath == cwdir {
		// check if pwd is filesystem root
		if err := cli.CheckFilesystemBoundary(cwdir); err != nil {
			return errors.Wrap(err, "can't initialise hasura project in filesystem root")
		}
		// check if the current directory is already a hasura project
		if err := cli.CheckDirectoryForFiles(cwdir, cli.ConfigFile); err == nil {
			return errors.Errorf("current working directory is already a hasura project directory")
		}
		o.EC.ExecutionDirectory = cwdir
		infoMsg = fmt.Sprintf(`hasura project initialised. execute the following command to continue:
  hasura console
`)
	} else {
		// create execution directory
		err := o.createExecutionDirectory()
		if err != nil {
			return err
		}
		infoMsg = fmt.Sprintf(`directory created. execute the following commands to continue:

  cd %s
  hasura console
`, o.EC.ExecutionDirectory)
	}

	// create template files
	err = o.createTemplateFiles()
	if err != nil {
		return err
	}

	// create other required files, like config.yaml, migrations directory
	err = o.createFiles()
	if err != nil {
		return err
	}

	o.EC.Logger.Info(infoMsg)
	return nil
}

//create the execution directory
func (o *InitOptions) createExecutionDirectory() error {
	if o.EC.ExecutionDirectory == "" {
		o.EC.ExecutionDirectory = o.InitDir
	} else {
		o.EC.ExecutionDirectory = filepath.Join(o.EC.ExecutionDirectory, o.InitDir)
	}

	// create the execution directory
	if _, err := os.Stat(o.EC.ExecutionDirectory); err == nil {
		return errors.Errorf("directory '%s' already exist", o.EC.ExecutionDirectory)
	}
	err := os.MkdirAll(o.EC.ExecutionDirectory, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "error creating setup directories")
	}

	return nil
}

// createFiles creates files required by the CLI in the ExecutionDirectory
func (o *InitOptions) createFiles() error {
	// create the directory
	err := os.MkdirAll(filepath.Dir(o.EC.ExecutionDirectory), os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "error creating setup directories")
	}
	// set config object
	var config *cli.Config
	if o.Version == cli.V1 {
		config = &cli.Config{
			ServerConfig: cli.ServerConfig{
				Endpoint: "http://localhost:8080",
			},
		}
	} else {
		config = &cli.Config{
			Version: o.Version,
			ServerConfig: cli.ServerConfig{
				Endpoint: "http://localhost:8080",
			},
			MetadataDirectory: "metadata",
			ActionConfig: &types.ActionExecutionConfig{
				Kind:                  "synchronous",
				HandlerWebhookBaseURL: "http://localhost:3000",
			},
		}
	}
	if o.Endpoint != "" {
		if _, err := url.ParseRequestURI(o.Endpoint); err != nil {
			return errors.Wrap(err, "error validating endpoint URL")
		}
		config.ServerConfig.Endpoint = o.Endpoint
	}
	if o.AdminSecret != "" {
		config.ServerConfig.AdminSecret = o.AdminSecret
	}

	// write the config file
	o.EC.Config = config
	o.EC.ConfigFile = filepath.Join(o.EC.ExecutionDirectory, "config.yaml")
	err = o.EC.WriteConfig(nil)
	if err != nil {
		return errors.Wrap(err, "cannot write config file")
	}

	// create migrations directory
	o.EC.MigrationDir = filepath.Join(o.EC.ExecutionDirectory, "migrations")
	err = os.MkdirAll(o.EC.MigrationDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot write migration directory")
	}

	if config.Version == cli.V2 {
		// create metadata directory
		o.EC.MetadataDir = filepath.Join(o.EC.ExecutionDirectory, "metadata")
		err = os.MkdirAll(o.EC.MetadataDir, os.ModePerm)
		if err != nil {
			return errors.Wrap(err, "cannot write migration directory")
		}

		// create metadata files
		plugins := make(metadataTypes.MetadataPlugins, 0)
		plugins = append(plugins, metadataVersion.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, tables.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, functions.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, querycollections.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, allowlist.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, remoteschemas.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, actions.New(o.EC, o.EC.MetadataDir))
		for _, plg := range plugins {
			err := plg.CreateFiles()
			if err != nil {
				return errors.Wrap(err, "cannot create metadata files")
			}
		}
	}
	return nil
}

func (o *InitOptions) createTemplateFiles() error {
	if o.Template == "" {
		return nil
	}
	err := o.EC.InitTemplatesRepo.EnsureUpdated()
	if err != nil {
		return errors.Wrap(err, "error in updating init-templates repo")
	}
	templatePath := filepath.Join(o.EC.InitTemplatesRepo.Path, o.Template)
	info, err := os.Stat(templatePath)
	if err != nil {
		return errors.Wrap(err, "template doesn't exists")
	}
	if !info.IsDir() {
		return errors.Errorf("template should be a directory")
	}
	err = util.CopyDir(templatePath, filepath.Join(o.EC.ExecutionDirectory, "install-manifest"))
	if err != nil {
		return err
	}
	return nil
}

func handlePromptError(err error) error {
	if err == promptui.ErrInterrupt {
		return errors.New("cancelled by user")
	}
	return errors.Wrap(err, "prompt failed")
}
