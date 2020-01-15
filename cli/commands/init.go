package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/metadata/allowlist"
	"github.com/hasura/graphql-engine/cli/metadata/functions"
	"github.com/hasura/graphql-engine/cli/metadata/querycollections"
	"github.com/hasura/graphql-engine/cli/metadata/remoteschemas"
	"github.com/hasura/graphql-engine/cli/metadata/tables"
	metadataVersion "github.com/hasura/graphql-engine/cli/metadata/version"
	hasuradbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"github.com/hasura/graphql-engine/cli/util"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/manifoldco/promptui"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const (
	defaultDirectory string = "hasura"
	initTemplatesURI        = "https://github.com/wawhal/graphql-engine-install-manifests.git"
)

// NewInitCmd is the definition for init command
func NewInitCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &initOptions{
		EC: ec,
	}
	initCmd := &cobra.Command{
		Use:   "init",
		Short: "Initialize directory for Hasura GraphQL Engine migrations",
		Long:  "Create directories and files required for enabling migrations on Hasura GraphQL Engine",
		Example: `  # Create a directory to store migrations
  hasura init

  # Now, edit <my-directory>/config.yaml to add endpoint and admin secret

  # Create a directory with endpoint and admin secret configured:
  hasura init --directory <my-project> --endpoint https://my-graphql-engine.com --admin-secret adminsecretkey

  # See https://docs.hasura.io/1.0/graphql/manual/migrations/index.html for more details`,
		SilenceUsage: true,
		PreRun: func(cmd *cobra.Command, args []string) {
			ec.Viper = viper.New()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := initCmd.Flags()
	f.StringVar(&opts.InitDir, "directory", "", "name of directory where files will be created")
	f.StringVar(&opts.MetadataDir, "metadata-directory", "metadata", "name of directory where metadata files will be created")
	f.StringVar(&opts.Endpoint, "endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "access-key", "", "access key for Hasura GraphQL Engine")
	f.StringVar(&opts.ActionKind, "action-kind", "synchronous", "")
	f.StringVar(&opts.ActionHandler, "action-handler", "http://localhost:3000", "")
	f.StringVar(&opts.Template, "install-manifest", "", "")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	return initCmd
}

type initOptions struct {
	EC *cli.ExecutionContext

	Endpoint    string
	AdminSecret string
	InitDir     string
	MetadataDir string

	ActionKind    string
	ActionHandler string

	Template string
}

func (o *initOptions) run() error {
	var dir string
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
			dir = r
		} else {
			dir = defaultDirectory
		}
	} else {
		dir = o.InitDir
	}

	if o.EC.ExecutionDirectory == "" {
		o.EC.ExecutionDirectory = dir
	} else {
		o.EC.ExecutionDirectory = filepath.Join(o.EC.ExecutionDirectory, dir)
	}

	var infoMsg string

	// create the execution directory
	if _, err := os.Stat(o.EC.ExecutionDirectory); err == nil {
		return errors.Errorf("directory '%s' already exist", o.EC.ExecutionDirectory)
	}
	err := os.MkdirAll(o.EC.ExecutionDirectory, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "error creating setup directories")
	}
	infoMsg = fmt.Sprintf(`directory created. execute the following commands to continue:

  cd %s
  hasura console
`, o.EC.ExecutionDirectory)

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

// createFiles creates files required by the CLI in the ExecutionDirectory
func (o *initOptions) createFiles() error {
	// create the directory
	err := os.MkdirAll(filepath.Dir(o.EC.ExecutionDirectory), os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "error creating setup directories")
	}
	// set config object
	config := &cli.Config{
		Version: "2",
		ServerConfig: cli.ServerConfig{
			Endpoint: "http://localhost:8080",
		},
		MetadataDirectory: o.MetadataDir,
		Action: actions.ActionExecutionConfig{
			Kind:                  o.ActionKind,
			HandlerWebhookBaseURL: o.ActionHandler,
		},
	}
	if o.Endpoint != "" {
		config.ServerConfig.Endpoint = o.Endpoint
	}
	if o.AdminSecret != "" {
		config.ServerConfig.AdminSecret = o.AdminSecret
	}

	// write the config file
	data, err := yaml.Marshal(config)
	if err != nil {
		return errors.Wrap(err, "cannot convert to yaml")
	}
	o.EC.ConfigFile = filepath.Join(o.EC.ExecutionDirectory, "config.yaml")
	err = ioutil.WriteFile(o.EC.ConfigFile, data, 0644)
	if err != nil {
		return errors.Wrap(err, "cannot write config file")
	}

	// create migrations directory
	o.EC.MigrationDir = filepath.Join(o.EC.ExecutionDirectory, "migrations")
	err = os.MkdirAll(o.EC.MigrationDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot write migration directory")
	}

	// create metadata directory
	o.EC.MetadataDir = filepath.Join(o.EC.ExecutionDirectory, "metadata")
	err = os.MkdirAll(o.EC.MetadataDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot write migration directory")
	}

	// TODO: import the packages and do a init to register has metadata plugins
	// create metadata files
	plugins := hasuradbTypes.MetadataPlugins{}
	plugins["version"] = metadataVersion.New(ec.MetadataDir)
	plugins["tables"] = tables.New(ec.MetadataDir)
	plugins["functions"] = functions.New(ec.MetadataDir)
	plugins["query_collections"] = querycollections.New(ec.MetadataDir)
	plugins["allow_list"] = allowlist.New(ec.MetadataDir)
	plugins["remote_schemas"] = remoteschemas.New(ec.MetadataDir)
	plugins["actions"] = actions.New(ec.MetadataDir, ec.Config.Action, ec.CMDName)
	for _, plg := range plugins {
		err := plg.CreateFiles()
		if err != nil {
			return errors.Wrap(err, "cannot create metadata files")
		}
	}
	return nil
}

func (o *initOptions) createTemplateFiles() error {
	if o.Template == "" {
		return nil
	}
	gitPath := filepath.Join(o.EC.GlobalConfigDir, "init-templates")
	git := util.NewGitUtil(initTemplatesURI, gitPath, "")
	err := git.EnsureUpdated()
	if err != nil {
		return err
	}
	templatePath := filepath.Join(gitPath, o.Template)
	info, err := os.Stat(templatePath)
	if err != nil {
		return errors.Wrap(err, "template doesn't exists")
	}
	if !info.IsDir() {
		return errors.Errorf("template should be a directory")
	}
	contents, err := ioutil.ReadDir(templatePath)
	if err != nil {
		return err
	}
	for _, content := range contents {
		cs, cd := filepath.Join(templatePath, content.Name()), filepath.Join(o.EC.ExecutionDirectory, "install-manifest", content.Name())
		// TODO: do we need *.md files?
		if strings.ToLower(filepath.Ext(cs)) == ".md" {
			continue
		}
		err = util.CopyFile(cs, cd)
		if err != nil {
			return err
		}
	}
	return nil
}

func handlePromptError(err error) error {
	if err == promptui.ErrInterrupt {
		return errors.New("cancelled by user")
	}
	return errors.Wrap(err, "prompt failed")
}
