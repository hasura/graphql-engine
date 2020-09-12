package cli

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/sirupsen/logrus"

	"github.com/gofrs/uuid"
	homedir "github.com/mitchellh/go-homedir"
	"github.com/pkg/errors"
	"github.com/spf13/viper"
)

// Environment defines the environment the CLI is running
type Environment string

const (
	// DefaultEnvironment - CLI running in default mode
	DefaultEnvironment Environment = "default"
	// ServerOnDockerEnvironment - CLI running in cli-migrations image
	ServerOnDockerEnvironment = "server-on-docker"
)

// GlobalConfig is the configuration object stored in the GlobalConfigFile.
type GlobalConfig struct {
	// UUID used for telemetry, generated on first run.
	UUID string `json:"uuid"`

	// Indicate if telemetry is enabled or not
	EnableTelemetry bool `json:"enable_telemetry"`

	// Indicates whether update notifications should be shown or not
	ShowUpdateNotification bool `json:"show_update_notification"`

	// CLIEnvironment defines the environment the CLI is running
	CLIEnvironment Environment `json:"cli_environment"`
}

type rawGlobalConfig struct {
	UUID                   *string     `json:"uuid"`
	EnableTelemetry        *bool       `json:"enable_telemetry"`
	ShowUpdateNotification *bool       `json:"show_update_notification"`
	CLIEnvironment         Environment `json:"cli_environment"`

	logger      *logrus.Logger
	shoudlWrite bool
}

func (c *rawGlobalConfig) read(filename string) error {
	b, err := ioutil.ReadFile(filename)
	if err != nil {
		return errors.Wrap(err, "read file")
	}
	err = json.Unmarshal(b, c)
	if err != nil {
		return errors.Wrap(err, "parse file")
	}
	return nil
}

func (c *rawGlobalConfig) validateKeys() error {
	// check prescence of uuid, create if doesn't exist
	if c.UUID == nil {
		u, err := uuid.NewV4()
		if err != nil {
			errors.Wrap(err, "failed generating uuid")
		}
		uid := u.String()
		c.UUID = &uid
		c.shoudlWrite = true
	}

	// check enabletelemetry
	if c.EnableTelemetry == nil {
		trueVal := true
		c.EnableTelemetry = &trueVal
		c.shoudlWrite = true
	}

	// check showupdatenotification
	if c.ShowUpdateNotification == nil {
		trueVal := true
		c.ShowUpdateNotification = &trueVal
		c.shoudlWrite = true
	}

	if c.CLIEnvironment == "" {
		c.CLIEnvironment = DefaultEnvironment
	}

	return nil
}

func (c *rawGlobalConfig) write(filename string) error {
	b, err := json.MarshalIndent(c, "", "  ")
	if err != nil {
		return errors.Wrap(err, "marshal file")
	}
	err = ioutil.WriteFile(filename, b, 0644)
	if err != nil {
		return errors.Wrap(err, "write file")
	}
	return nil
}

// setupGlobConfig ensures that global config directory and file exists and
// reads it into the GlobalConfig object.
func (ec *ExecutionContext) setupGlobalConfig() error {
	// check if the directory name is set, else default
	if len(ec.GlobalConfigDir) == 0 {
		ec.Logger.Debug("global config directory is not pre-set, defaulting")
		home, err := homedir.Dir()
		if err != nil {
			return errors.Wrap(err, "cannot get home directory")
		}
		globalConfigDir := filepath.Join(home, GlobalConfigDirName)
		ec.GlobalConfigDir = globalConfigDir
		ec.Logger.Debugf("global config directory set as '%s'", ec.GlobalConfigDir)
	}

	// create the config directory
	err := os.MkdirAll(ec.GlobalConfigDir, os.ModePerm)
	if err != nil {
		return errors.Wrap(err, "cannot create global config directory")
	}

	// check if the filename is set, else default
	if len(ec.GlobalConfigFile) == 0 {
		ec.GlobalConfigFile = filepath.Join(ec.GlobalConfigDir, GlobalConfigFileName)
		ec.Logger.Debugf("global config file set as '%s'", ec.GlobalConfigFile)
	}

	// check if the global config file exist
	_, err = os.Stat(ec.GlobalConfigFile)
	if os.IsNotExist(err) {

		// file does not exist, teat as first run and create it
		ec.Logger.Debug("global config file does not exist, this could be the first run, creating it...")

		// create an empty config object
		gc := &rawGlobalConfig{}

		// populate the keys
		err := gc.validateKeys()
		if err != nil {
			return errors.Wrap(err, "setup global config object")
		}

		// write the file
		err = gc.write(ec.GlobalConfigFile)
		if err != nil {
			return errors.Wrap(err, "write global config file")
		}
		ec.Logger.Debugf("global config file written at '%s' with content '%v'", ec.GlobalConfigFile, gc)

		// also show a notice about telemetry
		ec.Logger.Info(StrTelemetryNotice)

	} else if os.IsExist(err) || err == nil {

		// file exists, verify contents
		ec.Logger.Debug("global config file exisits, verifying contents")

		// initialize the config object
		gc := rawGlobalConfig{}
		err := gc.read(ec.GlobalConfigFile)
		if err != nil {
			return errors.Wrap(err, "reading global config file failed")
		}

		// validate keys
		err = gc.validateKeys()
		if err != nil {
			return errors.Wrap(err, "validating global config file failed")
		}

		// write the file if there are any changes
		if gc.shoudlWrite {
			err := gc.write(ec.GlobalConfigFile)
			if err != nil {
				return errors.Wrap(err, "writing global config file failed")
			}
			ec.Logger.Debugf("global config file written at '%s' with content '%+#v'", ec.GlobalConfigFile, gc)
		}

	}
	return ec.readGlobalConfig()
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
	v.SetDefault("cli_environment", DefaultEnvironment)
	err := v.ReadInConfig()
	if err != nil {
		return errors.Wrap(err, "cannot read global config from file/env")
	}
	if ec.GlobalConfig == nil {
		ec.Logger.Debugf("global config is not pre-set, reading from current env")
		ec.GlobalConfig = &GlobalConfig{
			UUID:                   v.GetString("uuid"),
			EnableTelemetry:        v.GetBool("enable_telemetry"),
			ShowUpdateNotification: v.GetBool("show_update_notification"),
			CLIEnvironment:         Environment(v.GetString("cli_environment")),
		}
	} else {
		ec.Logger.Debugf("global config is pre-set to %#v", ec.GlobalConfig)
	}
	ec.Logger.Debugf("global config: uuid: %v", ec.GlobalConfig.UUID)
	ec.Logger.Debugf("global config: enableTelemetry: %v", ec.GlobalConfig.EnableTelemetry)
	ec.Logger.Debugf("global config: showUpdateNotification: %v", ec.GlobalConfig.ShowUpdateNotification)
	ec.Logger.Debugf("global config: cliEnvironment: %v", ec.GlobalConfig.CLIEnvironment)

	// set if telemetry can be beamed or not
	ec.Telemetry.CanBeam = ec.GlobalConfig.EnableTelemetry
	ec.Telemetry.UUID = ec.GlobalConfig.UUID
	return nil
}
