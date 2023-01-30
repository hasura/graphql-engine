package cli

import (
	"encoding/json"
	stderrors "errors"
	"fmt"
	"io/fs"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/gofrs/uuid"
	homedir "github.com/mitchellh/go-homedir"
	"github.com/spf13/viper"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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

	shoudlWrite bool
}

func (c *rawGlobalConfig) read(filename string) error {
	var op errors.Op = "cli.rawGlobalConfig.read"
	b, err := ioutil.ReadFile(filename)
	if err != nil {
		return errors.E(op, fmt.Errorf("read file: %w", err))
	}
	err = json.Unmarshal(b, c)
	if err != nil {
		return errors.E(op, fmt.Errorf("parse file %w", err))
	}
	return nil
}

func (c *rawGlobalConfig) validateKeys() error {
	var op errors.Op = "cli.rawGlobalConfig.validateKeys"
	// check prescence of uuid, create if doesn't exist
	if c.UUID == nil {
		u, err := uuid.NewV4()
		if err != nil {
			return errors.E(op, fmt.Errorf("failed generating uuid : %w", err))
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
	var op errors.Op = "cli.rawGlobalConfig.write"
	b, err := json.MarshalIndent(c, "", "  ")
	if err != nil {
		return errors.E(op, fmt.Errorf("marshal file: %w", err))
	}
	err = ioutil.WriteFile(filename, b, 0644)
	if err != nil {
		return errors.E(op, fmt.Errorf("write file: %w", err))
	}
	return nil
}

// setupGlobConfig ensures that global config directory and file exists and
// reads it into the GlobalConfig object.
func (ec *ExecutionContext) setupGlobalConfig() error {
	var op errors.Op = "cli.ExecutionContext.setupGlobalConfig"
	// check if the directory name is set, else default
	if len(ec.GlobalConfigDir) == 0 {
		ec.Logger.Debug("global config directory is not pre-set, defaulting")
		home, err := homedir.Dir()
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot get home directory: %w", err))
		}
		globalConfigDir := filepath.Join(home, GlobalConfigDirName)
		ec.GlobalConfigDir = globalConfigDir
		ec.Logger.Debugf("global config directory set as '%s'", ec.GlobalConfigDir)
	}

	// create the config directory
	err := os.MkdirAll(ec.GlobalConfigDir, os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot create global config directory: %w", err))
	}

	// check if the filename is set, else default
	if len(ec.GlobalConfigFile) == 0 {
		ec.GlobalConfigFile = filepath.Join(ec.GlobalConfigDir, GlobalConfigFileName)
		ec.Logger.Debugf("global config file set as '%s'", ec.GlobalConfigFile)
	}

	// check if the global config file exist
	_, err = os.Stat(ec.GlobalConfigFile)
	if stderrors.Is(err, fs.ErrNotExist) {

		// file does not exist, teat as first run and create it
		ec.Logger.Debug("global config file does not exist, this could be the first run, creating it...")

		// create an empty config object
		gc := &rawGlobalConfig{}

		// populate the keys
		err := gc.validateKeys()
		if err != nil {
			return errors.E(op, fmt.Errorf("setup global config object: %w", err))
		}

		// write the file
		err = gc.write(ec.GlobalConfigFile)
		if err != nil {
			return errors.E(op, fmt.Errorf("write global config file: %w", err))
		}
		ec.Logger.Debugf("global config file written at '%s' with content '%v'", ec.GlobalConfigFile, gc)

		// also show a notice about telemetry
		ec.Logger.Info(TelemetryNotice)

	} else if stderrors.Is(err, fs.ErrExist) || err == nil {

		// file exists, verify contents
		ec.Logger.Debug("global config file exists, verifying contents")

		// initialize the config object
		gc := rawGlobalConfig{}
		err := gc.read(ec.GlobalConfigFile)
		if err != nil {
			return errors.E(op, fmt.Errorf("reading global config file failed: %w", err))
		}

		// validate keys
		err = gc.validateKeys()
		if err != nil {
			return errors.E(op, fmt.Errorf("validating global config file failed: %w", err))
		}

		// write the file if there are any changes
		if gc.shoudlWrite {
			err := gc.write(ec.GlobalConfigFile)
			if err != nil {
				return errors.E(op, fmt.Errorf("writing global config file failed: %w", err))
			}
			ec.Logger.Debugf("global config file written at '%s' with content '%+#v'", ec.GlobalConfigFile, gc)
		}

	}
	err = ec.readGlobalConfig()
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

// readGlobalConfig reads the configuration from global config file env vars,
// through viper.
func (ec *ExecutionContext) readGlobalConfig() error {
	var op errors.Op = "cli.ExecutionContext.readGlobalConfig"
	// need to get existing viper because https://github.com/spf13/viper/issues/233
	v := viper.New()
	v.SetEnvPrefix("HASURA_GRAPHQL")
	v.AutomaticEnv()
	v.SetConfigName("config")
	v.AddConfigPath(ec.GlobalConfigDir)
	v.SetDefault("cli_environment", DefaultEnvironment)
	err := v.ReadInConfig()
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot read global config from file/env: %w", err))
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
