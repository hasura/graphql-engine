package cli

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/gofrs/uuid"
	homedir "github.com/mitchellh/go-homedir"
	"github.com/pkg/errors"
	"github.com/spf13/viper"
)

// GlobalConfig is the configuration object stored in the GlobalConfigFile.
type GlobalConfig struct {
	// UUID used for telemetry, generated on first run.
	UUID string `json:"uuid"`

	// Indicate if telemetry is enabled or not
	EnableTelemetry bool `json:"enable_telemetry"`

	// Indicates whether update notifications should be shown or not
	ShowUpdateNotification bool `json:"show_update_notification"`
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
			UUID:                   u.String(),
			EnableTelemetry:        true,
			ShowUpdateNotification: true,
		}
		data, err := json.MarshalIndent(gc, "", "  ")
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
			UUID:            v.GetString("uuid"),
			EnableTelemetry: v.GetBool("enable_telemetry"),
		}
	} else {
		ec.Logger.Debugf("global config is pre-set to %#v", ec.GlobalConfig)
	}
	ec.Logger.Debugf("global config: uuid: %v", ec.GlobalConfig.UUID)
	ec.Logger.Debugf("global config: enableTelemetry: %v", ec.GlobalConfig.EnableTelemetry)
	// set if telemetry can be beamed or not
	ec.Telemetry.CanBeam = ec.GlobalConfig.EnableTelemetry
	ec.Telemetry.UUID = ec.GlobalConfig.UUID
	return nil
}
