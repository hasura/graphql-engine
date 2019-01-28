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
		Use:   "init",
		Short: "Initialize directory for Hasura GraphQL Engine migrations",
		Long:  "Create directories and files required for enabling migrations on Hasura GraphQL Engine",
		Example: `  # Create a directory to store migrations
  hasura init

  # Now, edit <my-directory>/config.yaml to add endpoint and access key

  # Create a directory with endpoint and access key configured:
  hasura init --directory <my-project> --endpoint https://my-graphql-engine.com --access-key secretaccesskey

  # See https://docs.hasura.io/1.0/graphql/manual/migrations/index.html for more details`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := initCmd.Flags()
	f.StringVar(&opts.InitDir, "directory", "", "name of directory where files will be created")
	f.StringVar(&opts.Endpoint, "endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.StringVar(&opts.AccessKey, "access-key", "", "access key for Hasura GraphQL Engine")
	return initCmd
}

type initOptions struct {
	EC *cli.ExecutionContext

	Endpoint  string
	AccessKey string
	InitDir   string
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
	config := &cli.HasuraGraphQLConfig{
		Endpoint: "http://localhost:8080",
	}
	if o.Endpoint != "" {
		config.Endpoint = o.Endpoint
	}
	if o.AccessKey != "" {
		config.AccessKey = o.AccessKey
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
