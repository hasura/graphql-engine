package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/manifoldco/promptui"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const (
	defaultDirectory = "hasura"
	MANIFESTS_DIR    = "install-scripts"
)

func NewInitCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &initOptions{
		EC: ec,
	}
	initCmd := &cobra.Command{
		Use:   "init",
		Short: "Initialize directory for Hasura GraphQL Engine",
		Long:  "Create directories and files required for Hasura GraphQL Engine",
		Example: `  # Create a directory with installation manifests:
  hasura init --directory <my-directory>

  # Create a directory with an endpoint configured:
  hasura --directory <my-directory> --endpoint <graphql-engine-endpoint>

  # See https://docs.hasura.io/0.15/graphql/manual/getting-started for more details`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			err := ec.Prepare()
			if err != nil {
				return errors.Wrap(err, "cmd prep failed")
			}
			return opts.Run()
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

func (o *initOptions) Run() error {
	if o.EC.ExecutionDirectory == "" {
		o.EC.ExecutionDirectory = o.InitDir
	}

	if len(o.InitDir) == 0 {
		p := promptui.Prompt{
			Label:   "Name of project directory ",
			Default: defaultDirectory,
		}
		r, err := p.Run()
		if err != nil {
			return handlePromptError(err)
		}
		if strings.TrimSpace(r) != "" {
			o.EC.ExecutionDirectory = r
		} else {
			o.EC.ExecutionDirectory = defaultDirectory
		}
	}

	var infoMsg string
	if len(o.Endpoint) == 0 {
		o.EC.Spin("Downloading install manifests... ")
		defer o.EC.Spinner.Stop()

		src, err := o.EC.InstallManifestsRepo.Download()
		if err != nil {
			return errors.Wrap(err, "getting manifests failed")
		}
		defer os.RemoveAll(src)
		o.EC.Spinner.Stop()
		o.EC.Logger.Info("Manifests downloaded")

		dst := filepath.Join(o.EC.ExecutionDirectory, MANIFESTS_DIR)

		if _, err := os.Stat(dst); err == nil {
			return errors.Errorf("directory '%s' already exist", dst)
		}

		o.EC.Spin("Creating manifests in current directory... ")
		defer o.EC.Spinner.Stop()
		err = os.MkdirAll(filepath.Dir(dst), os.ModePerm)
		if err != nil {
			return errors.Wrap(err, "error creating setup directories")
		}

		err = util.CopyDir(src, dst)
		if err != nil {
			return errors.Wrap(err, "error copying files")
		}
		o.EC.Spinner.Stop()
		infoMsg = fmt.Sprintf("manifests created at [%s]", dst)
	} else {
		if _, err := os.Stat(o.EC.ExecutionDirectory); err == nil {
			return errors.Errorf("directory '%s' already exist", o.EC.ExecutionDirectory)
		}
		err := os.MkdirAll(o.EC.ExecutionDirectory, os.ModePerm)
		if err != nil {
			return errors.Wrap(err, "error creating setup directories")
		}
		infoMsg = fmt.Sprintf(`directory created. execute the following commands to continue:

  cd %s
  %s console
`, o.EC.ExecutionDirectory, o.EC.CMDName)
	}

	err := o.createFiles()
	if err != nil {
		return err
	}

	o.EC.Logger.Infof(infoMsg)
	return nil
}

func (o *initOptions) createFiles() error {
	err := os.MkdirAll(filepath.Dir(o.EC.ExecutionDirectory), os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "error creating setup directories")
	}
	config := &cli.HasuraGraphQLConfig{
		Endpoint: "http://localhost:8080",
	}

	o.EC.ConfigFile = filepath.Join(o.EC.ExecutionDirectory, "config.yaml")

	// if o.Endpoint == "" {
	// 	 p := promptui.Prompt{
	// 	 	Label:   "Hasura GraphQL Engine endpoint",
	// 	 	Default: config.Endpoint,
	// 	 }
	// 	 r, err := p.Run()
	// 	 if err != nil {
	// 	 	return handlePromptError(err)
	// 	 }
	// 	 o.Endpoint = r
	// }
	if o.Endpoint == "" {
		o.Endpoint = config.Endpoint
	} else {
		config.Endpoint = o.Endpoint
	}

	data, err := yaml.Marshal(config)
	if err != nil {
		return errors.Wrap(err, "cannot convert to yaml")
	}
	err = ioutil.WriteFile(o.EC.ConfigFile, data, 0644)
	if err != nil {
		return errors.Wrap(err, "cannot write config file")
	}

	// if o.AccessKey == "" {
	// 	p := promptui.Prompt{
	// 		Label:   "Hasura GraphQL Engine access key",
	// 		Default: "<hasura-graphql-engine-access-key>",
	// 	}
	// 	r, err := p.Run()
	// 	if err != nil {
	// 		return handlePromptError(err)
	// 	}
	// 	o.AccessKey = r
	// }
	if o.AccessKey == "" {
		o.AccessKey = "<hasura-graphql-engine-access-key>"
	}
	err = ioutil.WriteFile(filepath.Join(o.EC.ExecutionDirectory, ".env"),
		[]byte(fmt.Sprintf("%s=%s", cli.ENV_ACCESS_KEY, o.AccessKey)), 0644)
	if err != nil {
		return errors.Wrap(err, "cannot write dotenv file")
	}

	o.EC.MigrationDir = filepath.Join(o.EC.ExecutionDirectory, "migrations")
	err = os.MkdirAll(o.EC.MigrationDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot write migration directory")
	}

	return nil
}

func getInstallManifests(url, target string) error {
	err := util.Download(url, target+".zip")
	if err != nil {
		return errors.Wrap(err, "failed downloading manifests")
	}
	err = os.RemoveAll(target)
	if err != nil {
		return errors.Wrap(err, "failed cleaning manifests")
	}
	err = util.Unzip(target+".zip", filepath.Dir(target))
	if err != nil {
		return errors.Wrap(err, "failed extracting manifests")
	}
	return nil
}

func handlePromptError(err error) error {
	if err == promptui.ErrInterrupt {
		return errors.New("cancelled by user")
	}
	return errors.Wrap(err, "prompt failed")
}
