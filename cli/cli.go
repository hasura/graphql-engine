package db

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"
	"time"

	"github.com/briandowns/spinner"
	"github.com/ghodss/yaml"
	"github.com/joho/godotenv"
	colorable "github.com/mattn/go-colorable"
	homedir "github.com/mitchellh/go-homedir"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const (
	ENV_ENDPOINT   = "HASURA_GRAPHQL_ENDPOINT"
	ENV_ACCESS_KEY = "HASURA_GRAPHQL_ACCESS_KEY"
)

type InstallManifestsRepo struct {
	Namespace string
	Name      string
}

func (i *InstallManifestsRepo) ZipURL() string {
	u := url.URL{
		Scheme: "https",
		Host:   "github.com",
		Path:   fmt.Sprintf("%s/%s/archive/master.zip", i.Namespace, i.Name),
	}
	return u.String()
}

func (i *InstallManifestsRepo) ZipExtractedDirectory() string {
	return fmt.Sprintf("%s-master", i.Name)
}

func (i *InstallManifestsRepo) GetReadmeURL() string {
	u := url.URL{
		Scheme: "https",
		Host:   "github.com",
		Path:   fmt.Sprintf("%s/%s/blob/master/README.md", i.Namespace, i.Name),
	}
	return u.String()
}

func (i *InstallManifestsRepo) GetDocsURL() string {
	return "https://docs.hasura.io/0.15/graphql/manual/getting-started"
}

type DatabaseConfig struct {
	Host         string
	Port         int32
	DatabaseName string
	Username     string
	Password     string
}

type HasuraGraphQLConfig struct {
	Endpoint  string `json:"endpoint"`
	AccessKey string `json:"access_key,omitempty"` // FIXME: should we have this here?
}

type ExecutionContext struct {
	CMDName string
	Spinner *spinner.Spinner
	Logger  *logrus.Logger

	ExecutionDirectory string
	MigrationDir       string
	ConfigFile         string
	EnvFile            string
	Config             *HasuraGraphQLConfig

	GlobalConfigDir           string
	GlobalConfigFile          string
	InstallManifestsDirectory string

	InstallManifestsRepo *InstallManifestsRepo

	IsStableRelease bool
}

func (ec *ExecutionContext) Spin(message string) {
	ec.Spinner.Stop()
	ec.Spinner.Prefix = message
	ec.Spinner.Start()
}

func (ec *ExecutionContext) Prepare() error {
	cmdName := os.Args[0]
	if len(cmdName) == 0 {
		cmdName = "hasura"
	}
	ec.CMDName = cmdName

	if ec.InstallManifestsRepo == nil {
		ec.InstallManifestsRepo = &InstallManifestsRepo{
			Name:      "graphql-engine-install-manifests",
			Namespace: "hasura",
		}
	}

	// set spinner
	ec.setupSpinner()

	// set logger
	ec.setupLogger()

	// setup config directory
	err := ec.setupGlobalConfigDir()
	if err != nil {
		return errors.Wrap(err, "setting up config directory failed")
	}

	return nil
}

func (ec *ExecutionContext) Validate() error {
	err := ec.Prepare()
	if err != nil {
		return err
	}

	// set directories and config
	err = ec.setupDirectory()
	if err != nil {
		return errors.Wrap(err, "setup")
	}

	ec.MigrationDir = filepath.Join(ec.ExecutionDirectory, "migrations")
	ec.ConfigFile = filepath.Join(ec.ExecutionDirectory, "config.yaml")
	ec.EnvFile = filepath.Join(ec.ExecutionDirectory, ".env")

	err = ec.readConfig()
	if err != nil {
		return errors.Wrap(err, "cannot read config")
	}

	return nil
}

func (ec *ExecutionContext) readConfigFromEnvVars() error {
	var config HasuraGraphQLConfig
	endpoint := os.Getenv(ENV_ENDPOINT)
	if len(endpoint) == 0 {
		return errors.Errorf("%s not set", ENV_ENDPOINT)
	}
	config.Endpoint = endpoint
	accessKey := os.Getenv(ENV_ACCESS_KEY)
	if len(accessKey) == 0 {
		return errors.Errorf("%s not set", ENV_ACCESS_KEY)
	}
	config.AccessKey = accessKey
	ec.Config = &config
	return nil
}

func (ec *ExecutionContext) readConfig() error {
	var config HasuraGraphQLConfig
	data, err := ioutil.ReadFile(ec.ConfigFile)
	if err != nil {
		return errors.Wrap(err, "cannot read config file")
	}
	err = yaml.Unmarshal(data, &config)
	if err != nil {
		return errors.Wrap(err, "cannot parse config file")
	}

	err = godotenv.Load(ec.EnvFile)
	if err != nil {
		if !os.IsNotExist(err) {
			return errors.Wrap(err, "Error loading .env file")
		}
	} else {
		accessKey := os.Getenv(ENV_ACCESS_KEY)
		if len(accessKey) != 0 {
			config.AccessKey = accessKey
		}
	}
	ec.Config = &config

	return nil
}

func (ec *ExecutionContext) setupSpinner() {
	if ec.Spinner == nil {
		spnr := spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		spnr.Writer = os.Stderr
		ec.Spinner = spnr
	}
}

func (ec *ExecutionContext) setupLogger() {
	if ec.Logger == nil {
		logger := logrus.New()
		logger.Formatter = &logrus.TextFormatter{
			ForceColors:      true,
			DisableTimestamp: true,
		}
		logger.Out = colorable.NewColorableStdout()
		ec.Logger = logger
	}
}

func (ec *ExecutionContext) setupGlobalConfigDir() error {
	if len(ec.GlobalConfigDir) == 0 {
		home, err := homedir.Dir()
		if err != nil {
			return errors.Wrap(err, "cannot get home directory")
		}
		globalConfigDir := filepath.Join(home, ".hasura-graphql")
		ec.GlobalConfigDir = globalConfigDir
	}
	err := os.MkdirAll(ec.GlobalConfigDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot create config directory")
	}
	ec.GlobalConfigFile = filepath.Join(ec.GlobalConfigDir, "config.json")
	ec.InstallManifestsDirectory = filepath.Join(ec.GlobalConfigDir, ec.InstallManifestsRepo.ZipExtractedDirectory())
	return nil
}

func (ec *ExecutionContext) setupDirectory() error {
	if len(ec.ExecutionDirectory) == 0 {
		cwd, err := os.Getwd()
		if err != nil {
			return errors.Wrap(err, "error getting current working directory")
		}
		ec.ExecutionDirectory = cwd
	}

	ed, err := os.Stat(ec.ExecutionDirectory)
	if err != nil {
		if os.IsNotExist(err) {
			return errors.Wrap(err, "did not find 'db/' directory. use 'init'?")
		} else {
			return errors.Wrap(err, "error getting directory details")
		}
	}
	if !ed.IsDir() {
		return errors.Errorf("'%s' is not a directory", ed.Name())
	}
	// config.yaml
	// migrations/
	// (optional) metadata.yaml
	dir, err := recursivelyValidateDirectory(ec.ExecutionDirectory)
	if err != nil {
		return errors.Wrap(err, "validate")
	}

	ec.ExecutionDirectory = dir
	return nil
}

func (ec *ExecutionContext) RenderText(in string) (out string) {
	t, err := template.New("r").Parse(in)
	if err != nil {
		ec.Logger.Debug(errors.Wrap(err, "failed parsing template"))
		return ""
	}
	b := &bytes.Buffer{}
	if err := t.Execute(b, ec); err != nil {
		ec.Logger.Debug(errors.Wrap(err, "failed rendering template"))
		return ""
	}
	return b.String()
}

var filesRequired = []string{
	"config.yaml",
	"migrations",
}

func recursivelyValidateDirectory(startFrom string) (validDir string, err error) {
	err = validateDirectory(startFrom)
	if err != nil {
		nextDir := filepath.Dir(startFrom)
		cleaned := filepath.Clean(nextDir)
		isWindowsRoot, _ := regexp.MatchString(`^[a-zA-Z]:\\$`, cleaned)
		// return error if filesystem boundary is hit
		if cleaned == "/" || isWindowsRoot {
			return nextDir, errors.Errorf("cannot find [%s] | search stopped at filesystem boundary", strings.Join(filesRequired, ", "))

		}
		return recursivelyValidateDirectory(nextDir)
	}
	return startFrom, nil
}

func validateDirectory(dir string) error {
	notFound := []string{}
	for _, f := range filesRequired {
		if _, err := os.Stat(filepath.Join(dir, f)); os.IsNotExist(err) {
			relpath, e := filepath.Rel(dir, f)
			if e == nil {
				f = relpath
			}
			notFound = append(notFound, f)
		}
	}
	if len(notFound) > 0 {
		return errors.Errorf("cannot validate directory '%s': [%s] not found", dir, strings.Join(notFound, ", "))
	}
	return nil
}
