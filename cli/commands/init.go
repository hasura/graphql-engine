package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/manifoldco/promptui"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const (
	defaultDirectory = "hasura"
)

// NewInitCmd is the definition for init command
func NewInitCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &initOptions{
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

  # See https://docs.hasura.io/1.0/graphql/manual/migrations/index.html for more details`,
		SilenceUsage: true,
		Args:         cobra.MaximumNArgs(1),
		PreRun: func(cmd *cobra.Command, args []string) {
			ec.Viper = viper.New()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if len(args) == 1 {
				opts.InitDir = args[0]
			}
			return opts.run()
		},
	}

	f := initCmd.Flags()
	f.StringVar(&opts.InitDir, "directory", "", "name of directory where files will be created")
	f.StringVar(&opts.Endpoint, "endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")
	f.MarkDeprecated("directory", "use directory-name argument instead")

	return initCmd
}

type initOptions struct {
	EC *cli.ExecutionContext

	Endpoint    string
	AdminSecret string
	InitDir     string
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
	config := &cli.ServerConfig{
		Endpoint: "http://localhost:8080",
	}
	if o.Endpoint != "" {
		config.Endpoint = o.Endpoint
	}
	if o.AdminSecret != "" {
		config.AdminSecret = o.AdminSecret
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

	return nil
}

func handlePromptError(err error) error {
	if err == promptui.ErrInterrupt {
		return errors.New("cancelled by user")
	}
	return errors.Wrap(err, "prompt failed")
}
