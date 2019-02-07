// Package cli and it's sub packages implements the command line tool for Hasura
// GraphQL Engine. The CLI operates on a directory, denoted by
// "ExecutionDirectory" in the "ExecutionContext" struct.
//
// The ExecutionContext is passed to all the subcommands so that a singleton
// context is available for the execution. Logger and Spinner comes from the same
// context.
package cli

import (
	"net/url"
	"os"
	"path/filepath"
	"time"

	"github.com/hasura/graphql-engine/cli/telemetry"
	"github.com/hasura/graphql-engine/cli/util"

	"github.com/briandowns/spinner"
	"github.com/gofrs/uuid"
	"github.com/hasura/graphql-engine/cli/version"
	colorable "github.com/mattn/go-colorable"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// Other constants used in the package
const (
	// Name of the global configuration directory
	GLOBAL_CONFIG_DIR_NAME = ".hasura"
	// Name of the global configuration file
	GLOBAL_CONFIG_FILE_NAME = "config.json"

	// Name of the file to store last update check time
	LastUpdateCheckFileName = "last_update_check_at"
)

// String constants
const (
	StrTelemetryNotice = `Help us improve Hasura! The cli collects anonymized usage stats which
allow us to keep improving Hasura at warp speed. To opt-out or read more,
visit https://docs.hasura.io/1.0/graphql/manual/guides/telemetry.html
`
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

	// ServerUUID is the unique ID for the server this execution is contacting.
	ServerUUID string

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

	// Telemetry collects the telemetry data throughout the execution
	Telemetry *telemetry.Data

	// LastUpdateCheckFile is the file where the timestamp of last update check is stored
	LastUpdateCheckFile string
}

// NewExecutionContext returns a new instance of execution context
func NewExecutionContext() *ExecutionContext {
	ec := &ExecutionContext{}
	ec.Telemetry = telemetry.BuildEvent()
	ec.Telemetry.Version = version.BuildVersion
	return ec
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
		return errors.Wrap(err, "setting up global config failed")
	}

	ec.LastUpdateCheckFile = filepath.Join(ec.GlobalConfigDir, LastUpdateCheckFileName)

	// initialize a blank server config
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
	ec.Telemetry.ExecutionID = ec.ID

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
	err = ec.checkServerVersion()
	if err != nil {
		return errors.Wrap(err, "version check")
	}

	state := util.GetServerState(ec.Config.Endpoint, ec.Config.AccessKey, ec.Version.ServerSemver, ec.Logger)
	ec.ServerUUID = state.UUID
	ec.Telemetry.ServerUUID = ec.ServerUUID
	ec.Logger.Debugf("server: uuid: %s", ec.ServerUUID)

	return nil
}

func (ec *ExecutionContext) checkServerVersion() error {
	v, err := version.FetchServerVersion(ec.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "failed to get version from server")
	}
	ec.Version.SetServerVersion(v)
	ec.Telemetry.ServerVersion = ec.Version.GetServerVersion()
	isCompatible, reason := ec.Version.CheckCLIServerCompatibility()
	ec.Logger.Debugf("versions: cli: [%s] server: [%s]", ec.Version.GetCLIVersion(), ec.Version.GetServerVersion())
	ec.Logger.Debugf("compatibility check: [%v] %v", isCompatible, reason)
	if !isCompatible {
		return errors.Errorf("[cli: %s] [server: %s] versions incompatible: %s", ec.Version.GetCLIVersion(), ec.Version.GetServerVersion(), reason)
	}
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

	// set the logger for telemetry
	if ec.Telemetry.Logger == nil {
		ec.Telemetry.Logger = ec.Logger
	}
}

// SetVersion sets the version inside context, according to the variable
// 'version' set during build context.
func (ec *ExecutionContext) setVersion() {
	if ec.Version == nil {
		ec.Version = version.New()
	}
}
