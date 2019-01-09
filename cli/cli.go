// Package cli and it's sub packages implements the command line tool for Hasura
// GraphQL Engine. The CLI operates on a directory, denoted by
// "ExecutionDirectory" in the "ExecutionContext" struct.
//
// The ExecutionContext is passed to all the subcommands so that a singleton
// context is available for the execution. Logger and Spinner comes from the same
// context.
package cli

import (
	"encoding/json"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	"github.com/gofrs/uuid"
	"github.com/hasura/graphql-engine/cli/version"
	colorable "github.com/mattn/go-colorable"
	homedir "github.com/mitchellh/go-homedir"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// Environment variable names recognised by the CLI.
const (
	// ENV_ENDPOINT is the name of env var which indicates the Hasura GraphQL
	// Engine endpoint URL.
	ENV_ENDPOINT = "HASURA_GRAPHQL_ENDPOINT"
	// ENV_ACCESS_KEY is the name of env var that has the access key for GraphQL
	// Engine endpoint.
	ENV_ACCESS_KEY = "HASURA_GRAPHQL_ACCESS_KEY"
)

// Other constants used in the package
const (
	// Name of the global configuration directory
	GLOBAL_CONFIG_DIR_NAME = ".hasura"
	// Name of the global configuration file
	GLOBAL_CONFIG_FILE_NAME = "config.json"
)

// String constants
const (
	StrTelemetryNotice = `To help improve Hasura, the CLI sends anonymous usage stats to the team.
The resulting data plays a key role in deciding what to focus on.
To read more and for instructions to opt-out, visit https://telemetry.hasura.io/faq`
)

// HasuraGraphQLConfig has the config values required to contact the server.
type HasuraGraphQLConfig struct {
	// Endpoint for the GraphQL Engine
	Endpoint string `json:"endpoint"`
	// AccessKey (optional) required to query the endpoint
	AccessKey string `json:"access_key,omitempty"`

	ParsedEndpoint *url.URL `json:"-"`
}

// ParseEndpoint ensures the endpoint is valid.
func (hgc *HasuraGraphQLConfig) ParseEndpoint() error {
	nurl, err := url.Parse(hgc.Endpoint)
	if err != nil {
		return err
	}
	hgc.ParsedEndpoint = nurl
	return nil
}

// GlobalConfig is the configuration object stored in the GlobalConfigFile.
type GlobalConfig struct {
	// UUID used for telemetry, generated on first run.
	UUID string `json:"uuid"`

	// Set to true if telemetry is disabled, can be set manually.
	DisableTelemetry bool `json:"disable_telemetry"`
}

// ExecutionContext contains various contextual information required by the cli
// at various points of it's execution. Values are filled in by the
// initializers and passed on to each command. Commands can also fill in values
// to be used further down the line.
type ExecutionContext struct {
	// CMDName is the name of CMD (os.Args[0]). To be filled in later to
	// correctly render example strings etc.
	CMDName string

	// ID is a unique ID for this Execution
	ID string

	// Spinner is the global spinner object used to show progress across the cli.
	Spinner *spinner.Spinner
	// Logger is the global logger object to print logs.
	Logger *logrus.Logger

	// ExecutionDirectory is the directory in which command is being executed.
	ExecutionDirectory string
	// MigrationDir is the name of directory where migrations are stored.
	MigrationDir string
	// ConfigFile is the file where endpoint etc. are stored.
	ConfigFile string
	// MetadataFile (optional) is a yaml file where Hasura metadata is stored.
	MetadataFile string

	// Config is the configuration object storing the endpoint and access key
	// information after reading from config file or env var.
	Config *HasuraGraphQLConfig

	// GlobalConfigDir is the ~/.hasura-graphql directory to store configuration
	// globally.
	GlobalConfigDir string
	// GlobalConfigFile is the file inside GlobalConfigDir where values are
	// stored.
	GlobalConfigFile string

	// GlobalConfig holds all the configuration options.
	GlobalConfig *GlobalConfig

	// IsStableRelease indicates if the CLI release is stable or not.
	IsStableRelease bool
	// Version indicates the version object
	Version *version.Version

	// Viper indicates the viper object for the execution
	Viper *viper.Viper

	// LogLevel indicates the logrus default logging level
	LogLevel string
}

// Prepare as the name suggests, prepares the ExecutionContext ec by
// initializing most of the variables to sensible defaults, if it is not already
// set.
func (ec *ExecutionContext) Prepare() error {
	// set the command name
	cmdName := os.Args[0]
	if len(cmdName) == 0 {
		cmdName = "hasura"
	}
	ec.CMDName = cmdName

	// set spinner
	ec.setupSpinner()

	// set logger
	ec.setupLogger()

	// populate version
	ec.setVersion()

	// setup global config
	err := ec.setupGlobalConfig()
	if err != nil {
		// TODO(shahidhk): should this be a failure?
		return errors.Wrap(err, "setting up global config directory failed")
	}

	// read global config
	err = ec.readGlobalConfig()
	if err != nil {
		return errors.Wrap(err, "reading global config failed")
	}

	// initialize a blank config
	if ec.Config == nil {
		ec.Config = &HasuraGraphQLConfig{}
	}

	// generate an execution id
	if ec.ID == "" {
		id := "00000000-0000-0000-0000-000000000000"
		u, err := uuid.NewV4()
		if err == nil {
			id = u.String()
		} else {
			ec.Logger.Debugf("generating uuid for execution ID failed, %v", err)
		}
		ec.ID = id
		ec.Logger.Debugf("execution id: %v", ec.ID)
	}

	return nil
}

// Validate prepares the ExecutionContext ec and then validates the
// ExecutionDirectory to see if all the required files and directories are in
// place.
func (ec *ExecutionContext) Validate() error {

	// validate execution directory
	err := ec.validateDirectory()
	if err != nil {
		return errors.Wrap(err, "validating current directory failed")
	}

	// set names of files and directories
	ec.MigrationDir = filepath.Join(ec.ExecutionDirectory, "migrations")
	ec.ConfigFile = filepath.Join(ec.ExecutionDirectory, "config.yaml")
	ec.MetadataFile = filepath.Join(ec.MigrationDir, "metadata.yaml")

	// read config and parse the values into Config
	err = ec.readConfig()
	if err != nil {
		return errors.Wrap(err, "cannot read config")
	}

	ec.Logger.Debug("graphql engine endpoint: ", ec.Config.Endpoint)
	ec.Logger.Debug("graphql engine access_key: ", ec.Config.AccessKey)

	// get version from the server and match with the cli version
	return ec.checkServerVersion()
}

func (ec *ExecutionContext) checkServerVersion() error {
	v, err := version.FetchServerVersion(ec.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "failed to get version from server")
	}
	ec.Version.SetServerVersion(v)
	isCompatible, reason := ec.Version.CheckCLIServerCompatibility()
	ec.Logger.Debugf("versions: cli: [%s] server: [%s]", ec.Version.GetCLIVersion(), ec.Version.GetServerVersion())
	ec.Logger.Debugf("compatibility check: [%v] %v", isCompatible, reason)
	if !isCompatible {
		return errors.Errorf("[cli: %s] [server: %s] versions incompatible: %s", ec.Version.GetCLIVersion(), ec.Version.GetServerVersion(), reason)
	}
	return nil
}

// readGlobalConfig reads the configuration from global config file env vars,
// through viper.
func (ec *ExecutionContext) readGlobalConfig() error {
	// need to get existing viper because https://github.com/spf13/viper/issues/233
	v := viper.New()
	v.SetEnvPrefix("HASURA_GRAPHQL")
	v.AutomaticEnv()
	v.SetConfigName("config")
	v.AddConfigPath(ec.GlobalConfigDir)
	err := v.ReadInConfig()
	if err != nil {
		return errors.Wrap(err, "cannor read global config from file/env")
	}
	if ec.GlobalConfig == nil {
		ec.Logger.Debugf("global config is not pre-set, reading from current env")
		ec.GlobalConfig = &GlobalConfig{
			UUID:             v.GetString("uuid"),
			DisableTelemetry: v.GetBool("disable_telemetry"),
		}
	} else {
		ec.Logger.Debugf("global config is pre-set to %#v", ec.GlobalConfig)
	}
	ec.Logger.Debugf("global config: uuid: %v", ec.GlobalConfig.UUID)
	ec.Logger.Debugf("global config: disable_telemetry: %v", ec.GlobalConfig.DisableTelemetry)
	return nil
}

// readConfig reads the configuration from config file, flags and env vars,
// through viper.
func (ec *ExecutionContext) readConfig() error {
	// need to get existing viper because https://github.com/spf13/viper/issues/233
	v := ec.Viper
	v.SetEnvPrefix("HASURA_GRAPHQL")
	v.AutomaticEnv()
	v.SetConfigName("config")
	v.SetDefault("endpoint", "http://localhost:8080")
	v.SetDefault("access_key", "")
	v.AddConfigPath(ec.ExecutionDirectory)
	err := v.ReadInConfig()
	if err != nil {
		return errors.Wrap(err, "cannor read config from file/env")
	}
	ec.Config = &HasuraGraphQLConfig{
		Endpoint:  v.GetString("endpoint"),
		AccessKey: v.GetString("access_key"),
	}
	return ec.Config.ParseEndpoint()
}

// setupSpinner creates a default spinner if the context does not already have
// one.
func (ec *ExecutionContext) setupSpinner() {
	if ec.Spinner == nil {
		spnr := spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		spnr.Writer = os.Stderr
		ec.Spinner = spnr
	}
}

// Spin stops any existing spinner and starts a new one with the given message.
func (ec *ExecutionContext) Spin(message string) {
	ec.Spinner.Stop()
	ec.Spinner.Prefix = message
	ec.Spinner.Start()
}

// setupLogger creates a default logger if context does not have one set.
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

	if ec.LogLevel != "" {
		level, err := logrus.ParseLevel(ec.LogLevel)
		if err != nil {
			ec.Logger.WithError(err).Error("error parsing log-level flag")
			return
		}
		ec.Logger.SetLevel(level)
	}
}

// setupGlobConfig ensures that global config directory and file exists and
// reads it into the GlobalConfig object.
func (ec *ExecutionContext) setupGlobalConfig() error {
	if len(ec.GlobalConfigDir) == 0 {
		ec.Logger.Debug("global config directory is not pre-set, defaulting")
		home, err := homedir.Dir()
		if err != nil {
			return errors.Wrap(err, "cannot get home directory")
		}
		globalConfigDir := filepath.Join(home, GLOBAL_CONFIG_DIR_NAME)
		ec.GlobalConfigDir = globalConfigDir
		ec.Logger.Debugf("global config directory set as '%s'", ec.GlobalConfigDir)
	}
	err := os.MkdirAll(ec.GlobalConfigDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot create global config directory")
	}
	if len(ec.GlobalConfigFile) == 0 {
		ec.GlobalConfigFile = filepath.Join(ec.GlobalConfigDir, GLOBAL_CONFIG_FILE_NAME)
		ec.Logger.Debugf("global config file set as '%s'", ec.GlobalConfigFile)
	}
	_, err = os.Stat(ec.GlobalConfigFile)
	if os.IsNotExist(err) {
		// file does not exist, teat as first run and create it
		ec.Logger.Debug("global config file does not exist, this could be the first run, creating it...")
		u, err := uuid.NewV4()
		if err != nil {
			return errors.Wrap(err, "failed to generate uuid")
		}
		gc := GlobalConfig{
			UUID:             u.String(),
			DisableTelemetry: false,
		}
		data, err := json.Marshal(gc)
		if err != nil {
			return errors.Wrap(err, "cannot marshal json for config file")
		}
		err = ioutil.WriteFile(ec.GlobalConfigFile, data, 0644)
		if err != nil {
			return errors.Wrap(err, "writing global config file failed")
		}
		ec.Logger.Debugf("global config file written at '%s' with content '%v'", ec.GlobalConfigFile, string(data))
		// also show a notice about telemetry
		ec.Logger.Info(StrTelemetryNotice)
	} else if os.IsExist(err) || err == nil {
		// file exists, verify contents
		ec.Logger.Debug("global config file exisits, verifying contents")
		data, err := ioutil.ReadFile(ec.GlobalConfigFile)
		if err != nil {
			return errors.Wrap(err, "reading global config file failed")
		}
		var gc GlobalConfig
		err = json.Unmarshal(data, &gc)
		if err != nil {
			return errors.Wrap(err, "global config file not a valid json")
		}
		_, err = uuid.FromString(gc.UUID)
		if err != nil {
			ec.Logger.Debugf("invalid uuid '%s' in global config: %v", gc.UUID, err)
			// create a new UUID
			ec.Logger.Debug("global config file exists, but uuid is invalid, creating a new one...")
			u, err := uuid.NewV4()
			if err != nil {
				return errors.Wrap(err, "failed to generate uuid")
			}
			gc.UUID = u.String()
			data, err := json.Marshal(gc)
			if err != nil {
				return errors.Wrap(err, "cannot marshal json for config file")
			}
			err = ioutil.WriteFile(ec.GlobalConfigFile, data, 0644)
			if err != nil {
				return errors.Wrap(err, "writing global config file failed")
			}
			ec.Logger.Debugf("global config file written at '%s' with content '%v'", ec.GlobalConfigFile, string(data))
		}

	}
	return nil
}

// validateDirectory sets execution directory and validate it to see that or any
// of the parent directory is a valid project directory. A valid project
// directory contains the following:
// 1. migrations directory
// 2. config.yaml file
// 3. metadata.yaml (optional)
// If the current directory or any parent directory (upto filesystem root) is
// found to have these files, ExecutionDirectory is set as that directory.
func (ec *ExecutionContext) validateDirectory() error {
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
			return errors.Wrap(err, "did not find required directory. use 'init'?")
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

// filesRequired are the files that are mandatory to qualify for a project
// directory.
var filesRequired = []string{
	"config.yaml",
	"migrations",
}

// recursivelyValidateDirectory tries to parse 'startFrom' as a project
// directory by checking for the 'filesRequired'. If the parent of 'startFrom'
// (nextDir) is filesystem root, error is returned. Otherwise, 'nextDir' is
// validated, recursively.
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

// validateDirectory tries to parse dir for the filesRequired and returns error
// if any one of them is missing.
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

// SetVersion sets the version inside context, according to the variable
// 'version' set during build context.
func (ec *ExecutionContext) setVersion() {
	if ec.Version == nil {
		ec.Version = version.New()
	}
}
