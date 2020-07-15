// Package cli and it's sub packages implements the command line tool for Hasura
// GraphQL Engine. The CLI operates on a directory, denoted by
// "ExecutionDirectory" in the "ExecutionContext" struct.
//
// The ExecutionContext is passed to all the subcommands so that a singleton
// context is available for the execution. Logger and Spinner comes from the same
// context.
package cli

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"time"

	"github.com/Masterminds/semver"
	"github.com/briandowns/spinner"
	"github.com/gofrs/uuid"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/hasura/graphql-engine/cli/telemetry"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
	"github.com/subosito/gotenv"
	"golang.org/x/crypto/ssh/terminal"
	"gopkg.in/yaml.v2"
)

// Other constants used in the package
const (
	// Name of the global configuration directory
	GlobalConfigDirName = ".hasura"
	// Name of the global configuration file
	GlobalConfigFileName = "config.json"

	// Name of the file to store last update check time
	LastUpdateCheckFileName = "last_update_check_at"

	// Name of the cli extension plugin
	CLIExtPluginName = "cli-ext"

	DefaultMigrationsDirectory = "migrations"
	DefaultMetadataDirectory   = "metadata"
	DefaultSeedsDirectory      = "seeds"
)

const (
	XHasuraAdminSecret = "X-Hasura-Admin-Secret"
	XHasuraAccessKey   = "X-Hasura-Access-Key"
)

// String constants
const (
	StrTelemetryNotice = `Help us improve Hasura! The cli collects anonymized usage stats which
allow us to keep improving Hasura at warp speed. To opt-out or read more,
visit https://hasura.io/docs/1.0/graphql/manual/guides/telemetry.html
`
)

// ConfigVersion defines the version of the Config.
type ConfigVersion int

const (
	// V1 represents config version 1
	V1 ConfigVersion = iota + 1
	// V2 represents config version 2
	V2
)

// ServerAPIPaths has the custom paths defined for server api
type ServerAPIPaths struct {
	Query   string `yaml:"query,omitempty"`
	GraphQL string `yaml:"graphql,omitempty"`
	Config  string `yaml:"config,omitempty"`
	PGDump  string `yaml:"pg_dump,omitempty"`
	Version string `yaml:"version,omitempty"`
}

// GetQueryParams - encodes the values in url
func (s ServerAPIPaths) GetQueryParams() url.Values {
	vals := url.Values{}
	t := reflect.TypeOf(s)
	for i := 0; i < t.NumField(); i++ {
		field := t.Field(i)
		tag := field.Tag.Get("yaml")
		splitTag := strings.Split(tag, ",")
		if len(splitTag) == 0 {
			continue
		}
		name := splitTag[0]
		if name == "-" {
			continue
		}
		v := reflect.ValueOf(s).Field(i)
		vals.Add(name, v.String())
	}
	return vals
}

// ErrInvalidConfigVersion - if the config version is not valid
var ErrInvalidConfigVersion error = fmt.Errorf("invalid config version")

// NewConfigVersionValue returns ConfigVersion set with default value
func NewConfigVersionValue(val ConfigVersion, p *ConfigVersion) *ConfigVersion {
	*p = val
	return p
}

// Set sets the value of the named command-line flag.
func (c *ConfigVersion) Set(s string) error {
	v, err := strconv.ParseInt(s, 0, 64)
	*c = ConfigVersion(v)
	if err != nil {
		return err
	}
	if !c.IsValid() {
		return ErrInvalidConfigVersion
	}
	return nil
}

// Type returns a string that uniquely represents this flag's type.
func (c *ConfigVersion) Type() string {
	return "int"
}

func (c *ConfigVersion) String() string {
	return strconv.Itoa(int(*c))
}

// IsValid returns if its a valid config version
func (c ConfigVersion) IsValid() bool {
	return c != 0 && c <= V2
}

// ServerConfig has the config values required to contact the server
type ServerConfig struct {
	// Endpoint for the GraphQL Engine
	Endpoint string `yaml:"endpoint"`
	// AccessKey (deprecated) (optional) Admin secret key required to query the endpoint
	AccessKey string `yaml:"access_key,omitempty"`
	// AdminSecret (optional) Admin secret required to query the endpoint
	AdminSecret string `yaml:"admin_secret,omitempty"`
	// APIPaths (optional) API paths for server
	APIPaths *ServerAPIPaths `yaml:"api_paths,omitempty"`
	// InsecureSkipTLSVerify - indicates if TLS verification is disabled or not.
	InsecureSkipTLSVerify bool `yaml:"insecure_skip_tls_verify,omitempty"`
	// CAPath - Path to a cert file for the certificate authority
	CAPath string `yaml:"certificate_authority,omitempty"`

	ParsedEndpoint *url.URL `yaml:"-"`

	TLSConfig *tls.Config `yaml:"-"`

	HTTPClient                 *http.Client               `yaml:"-"`
	HasuraServerInternalConfig HasuraServerInternalConfig `yaml:"-"`
}

func (c *ServerConfig) GetHasuraInternalServerConfig() error {
	// Determine from where assets should be served
	url := c.getConfigEndpoint()
	client := http.Client{Timeout: 30 * time.Second}
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return errors.Wrap(err, "error fetching config from server")
	}

	if c.AdminSecret != "" {
		req.Header.Set(XHasuraAdminSecret, c.AdminSecret)
	}

	r, err := client.Do(req)
	if err != nil {
		return err
	}
	defer r.Body.Close()

	if r.StatusCode != http.StatusOK {
		var horror hasuradb.HasuraError
		err := json.NewDecoder(r.Body).Decode(&horror)
		if err != nil {
			return fmt.Errorf("error fetching server config")
		}

		return fmt.Errorf("error fetching server config: %v", horror.Error())
	}

	return json.NewDecoder(r.Body).Decode(&c.HasuraServerInternalConfig)
}

// HasuraServerConfig is the type returned by the v1alpha1/config API
// TODO: Move this type to a client implementation for hasura
type HasuraServerInternalConfig struct {
	Version          string `json:"version"`
	IsAdminSecretSet bool   `json:"is_admin_secret_set"`
	IsAuthHookSet    bool   `json:"is_auth_hook_set"`
	IsJwtSet         bool   `json:"is_jwt_set"`
	JWT              string `json:"jwt"`
	ConsoleAssetsDir string `json:"console_assets_dir"`
}

// GetVersionEndpoint provides the url to contact the version API
func (s *ServerConfig) GetVersionEndpoint() string {
	nurl := *s.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, s.APIPaths.Version)
	return nurl.String()
}

// GetQueryEndpoint provides the url to contact the query API
func (s *ServerConfig) GetQueryEndpoint() string {
	nurl := *s.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, s.APIPaths.Query)
	return nurl.String()
}

// GetVersionEndpoint provides the url to contact the config API
func (s *ServerConfig) getConfigEndpoint() string {
	nurl := *s.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, s.APIPaths.Config)
	return nurl.String()
}

// ParseEndpoint ensures the endpoint is valid.
func (s *ServerConfig) ParseEndpoint() error {
	nurl, err := url.ParseRequestURI(s.Endpoint)
	if err != nil {
		return err
	}
	s.ParsedEndpoint = nurl
	return nil
}

// SetTLSConfig - sets the TLS config
func (s *ServerConfig) SetTLSConfig() error {
	if s.InsecureSkipTLSVerify {
		s.TLSConfig = &tls.Config{InsecureSkipVerify: true}
	}
	if s.CAPath != "" {
		// Get the SystemCertPool, continue with an empty pool on error
		rootCAs, _ := x509.SystemCertPool()
		if rootCAs == nil {
			rootCAs = x509.NewCertPool()
		}
		// read cert
		certPath, _ := filepath.Abs(s.CAPath)
		cert, err := ioutil.ReadFile(certPath)
		if err != nil {
			return errors.Errorf("error reading CA %s", s.CAPath)
		}
		if ok := rootCAs.AppendCertsFromPEM(cert); !ok {
			return errors.Errorf("Unable to append given CA cert.")
		}
		s.TLSConfig = &tls.Config{
			RootCAs:            rootCAs,
			InsecureSkipVerify: s.InsecureSkipTLSVerify,
		}
	}
	return nil
}

// SetHTTPClient - sets the http client
func (s *ServerConfig) SetHTTPClient() error {
	s.HTTPClient = &http.Client{Transport: http.DefaultTransport}
	if s.TLSConfig != nil {
		tr := &http.Transport{TLSClientConfig: s.TLSConfig}
		s.HTTPClient.Transport = tr
	}
	return nil
}

// Config represents configuration required for the CLI to function
type Config struct {
	// Version of the config.
	Version ConfigVersion `yaml:"version,omitempty"`

	// ServerConfig to be used by CLI to contact server.
	ServerConfig `yaml:",inline"`

	// MetadataDirectory defines the directory where the metadata files were stored.
	MetadataDirectory string `yaml:"metadata_directory,omitempty"`
	// MigrationsDirectory defines the directory where the migration files were stored.
	MigrationsDirectory string `yaml:"migrations_directory,omitempty"`
	// SeedsDirectory defines the directory where seed files will be stored
	SeedsDirectory string `yaml:"seeds_directory,omitempty"`
	// ActionConfig defines the config required to create or generate codegen for an action.
	ActionConfig *types.ActionExecutionConfig `yaml:"actions,omitempty"`
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
	// Envfile is the .env file to load ENV vars from
	Envfile string
	// MigrationDir is the name of directory where migrations are stored.
	MigrationDir string
	// MetadataDir is the name of directory where metadata files are stored.
	MetadataDir string
	// Seed directory -- directory in which seed files are to be stored
	SeedsDirectory string
	// ConfigFile is the file where endpoint etc. are stored.
	ConfigFile string
	// HGE Headers, are the custom headers which can be passed to HGE API
	HGEHeaders map[string]string

	// Config is the configuration object storing the endpoint and admin secret
	// information after reading from config file or env var.
	Config *Config

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

	// NoColor indicates if the outputs shouldn't be colorized
	NoColor bool

	// Telemetry collects the telemetry data throughout the execution
	Telemetry *telemetry.Data

	// LastUpdateCheckFile is the file where the timestamp of last update check is stored
	LastUpdateCheckFile string

	// SkipUpdateCheck will skip the auto update check if set to true
	SkipUpdateCheck bool

	// PluginsConfig defines the config for plugins
	PluginsConfig *plugins.Config

	// CodegenAssetsRepo defines the config to handle codegen-assets repo
	CodegenAssetsRepo *util.GitUtil

	// InitTemplatesRepo defines the config to handle init-templates repo
	InitTemplatesRepo *util.GitUtil

	// IsTerminal indicates whether the current session is a terminal or not
	IsTerminal bool
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

	ec.IsTerminal = terminal.IsTerminal(int(os.Stdout.Fd()))

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

	// setup plugins path
	err = ec.setupPlugins()
	if err != nil {
		return errors.Wrap(err, "setting up plugins path failed")
	}

	err = ec.setupCodegenAssetsRepo()
	if err != nil {
		return errors.Wrap(err, "setting up codegen-assets repo failed")
	}

	err = ec.setupInitTemplatesRepo()
	if err != nil {
		return errors.Wrap(err, "setting up init-templates repo failed")
	}

	ec.LastUpdateCheckFile = filepath.Join(ec.GlobalConfigDir, LastUpdateCheckFileName)

	// initialize a blank server config
	if ec.Config == nil {
		ec.Config = &Config{}
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

// setupPlugins create and returns the inferred paths for hasura. By default, it assumes
// $HOME/.hasura as the base path
func (ec *ExecutionContext) setupPlugins() error {
	base := filepath.Join(ec.GlobalConfigDir, "plugins")
	base, err := filepath.Abs(base)
	if err != nil {
		return errors.Wrap(err, "cannot get absolute path")
	}
	ec.PluginsConfig = plugins.New(base)
	ec.PluginsConfig.Logger = ec.Logger
	ec.PluginsConfig.Repo.Logger = ec.Logger
	if ec.GlobalConfig.CLIEnvironment == ServerOnDockerEnvironment {
		ec.PluginsConfig.Repo.DisableCloneOrUpdate = true
	}
	return ec.PluginsConfig.Prepare()
}

func (ec *ExecutionContext) setupCodegenAssetsRepo() error {
	base := filepath.Join(ec.GlobalConfigDir, util.ActionsCodegenDirName)
	base, err := filepath.Abs(base)
	if err != nil {
		return errors.Wrap(err, "cannot get absolute path")
	}
	ec.CodegenAssetsRepo = util.NewGitUtil(util.ActionsCodegenRepoURI, base, "")
	ec.CodegenAssetsRepo.Logger = ec.Logger
	if ec.GlobalConfig.CLIEnvironment == ServerOnDockerEnvironment {
		ec.CodegenAssetsRepo.DisableCloneOrUpdate = true
	}
	return nil
}

func (ec *ExecutionContext) setupInitTemplatesRepo() error {
	base := filepath.Join(ec.GlobalConfigDir, util.InitTemplatesDirName)
	base, err := filepath.Abs(base)
	if err != nil {
		return errors.Wrap(err, "cannot get absolute path")
	}
	ec.InitTemplatesRepo = util.NewGitUtil(util.InitTemplatesRepoURI, base, "")
	ec.InitTemplatesRepo.Logger = ec.Logger
	if ec.GlobalConfig.CLIEnvironment == ServerOnDockerEnvironment {
		ec.InitTemplatesRepo.DisableCloneOrUpdate = true
	}
	return nil
}

func (ec *ExecutionContext) SetHGEHeaders(headers map[string]string) {
	ec.HGEHeaders = headers
}

// Validate prepares the ExecutionContext ec and then validates the
// ExecutionDirectory to see if all the required files and directories are in
// place.
func (ec *ExecutionContext) Validate() error {
	// ensure plugins index exists
	err := ec.PluginsConfig.Repo.EnsureCloned()
	if err != nil {
		return errors.Wrap(err, "ensuring plugins index failed")
	}

	// ensure codegen-assets repo exists
	err = ec.CodegenAssetsRepo.EnsureCloned()
	if err != nil {
		return errors.Wrap(err, "ensuring codegen-assets repo failed")
	}

	// validate execution directory
	err = ec.validateDirectory()
	if err != nil {
		return errors.Wrap(err, "validating current directory failed")
	}

	// load .env file
	err = ec.loadEnvfile()
	if err != nil {
		return errors.Wrap(err, "loading .env file failed")
	}

	// set names of config file
	ec.ConfigFile = filepath.Join(ec.ExecutionDirectory, "config.yaml")

	// read config and parse the values into Config
	err = ec.readConfig()
	if err != nil {
		return errors.Wrap(err, "cannot read config")
	}

	// set name of migration directory
	ec.MigrationDir = filepath.Join(ec.ExecutionDirectory, ec.Config.MigrationsDirectory)
	if _, err := os.Stat(ec.MigrationDir); os.IsNotExist(err) {
		err = os.MkdirAll(ec.MigrationDir, os.ModePerm)
		if err != nil {
			return errors.Wrap(err, "cannot create migrations directory")
		}
	}

	ec.SeedsDirectory = filepath.Join(ec.ExecutionDirectory, ec.Config.SeedsDirectory)
	if _, err := os.Stat(ec.SeedsDirectory); os.IsNotExist(err) {
		err = os.MkdirAll(ec.SeedsDirectory, os.ModePerm)
		if err != nil {
			return errors.Wrap(err, "cannot create seeds directory")
		}
	}

	if ec.Config.Version == V2 && ec.Config.MetadataDirectory != "" {
		// set name of metadata directory
		ec.MetadataDir = filepath.Join(ec.ExecutionDirectory, ec.Config.MetadataDirectory)
		if _, err := os.Stat(ec.MetadataDir); os.IsNotExist(err) {
			err = os.MkdirAll(ec.MetadataDir, os.ModePerm)
			if err != nil {
				return errors.Wrap(err, "cannot create metadata directory")
			}
		}
	}

	ec.Logger.Debug("graphql engine endpoint: ", ec.Config.ServerConfig.Endpoint)
	ec.Logger.Debug("graphql engine admin_secret: ", ec.Config.ServerConfig.AdminSecret)

	// get version from the server and match with the cli version
	err = ec.checkServerVersion()
	if err != nil {
		return errors.Wrap(err, "version check")
	}

	// get the server feature flags
	err = ec.Version.GetServerFeatureFlags()
	if err != nil {
		return errors.Wrap(err, "error in getting server feature flags")
	}

	state := util.GetServerState(ec.Config.ServerConfig.GetQueryEndpoint(), ec.Config.ServerConfig.AdminSecret, ec.Config.ServerConfig.TLSConfig, ec.Version.ServerSemver, ec.Logger)
	ec.ServerUUID = state.UUID
	ec.Telemetry.ServerUUID = ec.ServerUUID
	ec.Logger.Debugf("server: uuid: %s", ec.ServerUUID)
	// Set headers required for communicating with HGE
	if ec.Config.AdminSecret != "" {
		headers := map[string]string{
			GetAdminSecretHeaderName(ec.Version): ec.Config.AdminSecret,
		}
		ec.SetHGEHeaders(headers)
	}
	return nil
}

func (ec *ExecutionContext) checkServerVersion() error {
	v, err := version.FetchServerVersion(ec.Config.ServerConfig.GetVersionEndpoint(), ec.Config.ServerConfig.HTTPClient)
	if err != nil {
		return errors.Wrap(err, "failed to get version from server")
	}
	ec.Version.SetServerVersion(v)
	ec.Telemetry.ServerVersion = ec.Version.GetServerVersion()
	isCompatible, reason := ec.Version.CheckCLIServerCompatibility()
	ec.Logger.Debugf("versions: cli: [%s] server: [%s]", ec.Version.GetCLIVersion(), ec.Version.GetServerVersion())
	ec.Logger.Debugf("compatibility check: [%v] %v", isCompatible, reason)
	if !isCompatible {
		ec.Logger.Warnf("[cli: %s] [server: %s] version mismatch: %s", ec.Version.GetCLIVersion(), ec.Version.GetServerVersion(), reason)
	}
	return nil
}

// WriteConfig writes the configuration from ec.Config or input config
func (ec *ExecutionContext) WriteConfig(config *Config) error {
	var cfg *Config
	if config != nil {
		cfg = config
	} else {
		cfg = ec.Config
	}
	y, err := yaml.Marshal(cfg)
	if err != nil {
		return err
	}
	return ioutil.WriteFile(ec.ConfigFile, y, 0644)
}

// readConfig reads the configuration from config file, flags and env vars,
// through viper.
func (ec *ExecutionContext) readConfig() error {
	// need to get existing viper because https://github.com/spf13/viper/issues/233
	v := ec.Viper
	v.SetEnvPrefix(util.ViperEnvPrefix)
	v.SetEnvKeyReplacer(util.ViperEnvReplacer)
	v.AutomaticEnv()
	v.SetConfigName("config")
	v.SetDefault("version", "1")
	v.SetDefault("endpoint", "http://localhost:8080")
	v.SetDefault("admin_secret", "")
	v.SetDefault("access_key", "")
	v.SetDefault("api_paths.query", "v1/query")
	v.SetDefault("api_paths.graphql", "v1/graphql")
	v.SetDefault("api_paths.config", "v1alpha1/config")
	v.SetDefault("api_paths.pg_dump", "v1alpha1/pg_dump")
	v.SetDefault("api_paths.version", "v1/version")
	v.SetDefault("metadata_directory", "")
	v.SetDefault("migrations_directory", DefaultMigrationsDirectory)
	v.SetDefault("seeds_directory", DefaultSeedsDirectory)
	v.SetDefault("actions.kind", "synchronous")
	v.SetDefault("actions.handler_webhook_baseurl", "http://localhost:3000")
	v.SetDefault("actions.codegen.framework", "")
	v.SetDefault("actions.codegen.output_dir", "")
	v.SetDefault("actions.codegen.uri", "")
	v.AddConfigPath(ec.ExecutionDirectory)
	err := v.ReadInConfig()
	if err != nil {
		return errors.Wrap(err, "cannot read config from file/env")
	}
	adminSecret := v.GetString("admin_secret")
	if adminSecret == "" {
		adminSecret = v.GetString("access_key")
	}

	ec.Config = &Config{
		Version: ConfigVersion(v.GetInt("version")),
		ServerConfig: ServerConfig{
			Endpoint:    v.GetString("endpoint"),
			AdminSecret: adminSecret,
			APIPaths: &ServerAPIPaths{
				Query:   v.GetString("api_paths.query"),
				GraphQL: v.GetString("api_paths.graphql"),
				Config:  v.GetString("api_paths.config"),
				PGDump:  v.GetString("api_paths.pg_dump"),
				Version: v.GetString("api_paths.version"),
			},
			InsecureSkipTLSVerify: v.GetBool("insecure_skip_tls_verify"),
			CAPath:                v.GetString("certificate_authority"),
		},
		MetadataDirectory:   v.GetString("metadata_directory"),
		MigrationsDirectory: v.GetString("migrations_directory"),
		SeedsDirectory:      v.GetString("seeds_directory"),
		ActionConfig: &types.ActionExecutionConfig{
			Kind:                  v.GetString("actions.kind"),
			HandlerWebhookBaseURL: v.GetString("actions.handler_webhook_baseurl"),
			Codegen: &types.CodegenExecutionConfig{
				Framework: v.GetString("actions.codegen.framework"),
				OutputDir: v.GetString("actions.codegen.output_dir"),
				URI:       v.GetString("actions.codegen.uri"),
			},
		},
	}
	if !ec.Config.Version.IsValid() {
		return ErrInvalidConfigVersion
	}
	err = ec.Config.ServerConfig.ParseEndpoint()
	if err != nil {
		return errors.Wrap(err, "unable to parse server endpoint")
	}

	// this populates the ec.Config.ServerConfig.HasuraServerInternalConfig
	err = ec.Config.ServerConfig.GetHasuraInternalServerConfig()
	if err != nil {
		// If config API is not enabled log it and don't fail
		ec.Logger.Debugf("cannot get config information from server, this might be because config API is not enabled: %v", err)
	}

	err = ec.Config.ServerConfig.SetTLSConfig()
	if err != nil {
		return errors.Wrap(err, "setting up TLS config failed")
	}
	return ec.Config.ServerConfig.SetHTTPClient()
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
	if ec.IsTerminal {
		ec.Spinner.Stop()
		ec.Spinner.Prefix = message
		ec.Spinner.Start()
	} else {
		ec.Logger.Println(message)
	}
}

// loadEnvfile loads .env file
func (ec *ExecutionContext) loadEnvfile() error {
	envfile := filepath.Join(ec.ExecutionDirectory, ec.Envfile)
	err := gotenv.Load(envfile)
	if err != nil {
		// return error if user provided envfile name
		if ec.Envfile != ".env" {
			return err
		}
		if !os.IsNotExist(err) {
			ec.Logger.Warn(err)
		}
	}
	if err == nil {
		ec.Logger.Debug("ENV vars read from: ", envfile)
	}
	return nil
}

// setupLogger creates a default logger if context does not have one set.
func (ec *ExecutionContext) setupLogger() {
	if ec.Logger == nil {
		logger := logrus.New()
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

	ec.Logger.Hooks = make(logrus.LevelHooks)
	ec.Logger.AddHook(newSpinnerHandlerHook(ec.Logger, ec.Spinner, ec.IsTerminal, ec.NoColor))

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

// InstallPlugin installs a plugin depending on forceCLIVersion.
// If forceCLIVersion is set, it uses ec.Version.CLISemver version for the plugin to be installed.
// Else, it installs the latest version of the plugin
func (ec ExecutionContext) InstallPlugin(name string, forceCLIVersion bool) error {
	var version *semver.Version
	if forceCLIVersion {
		err := ec.PluginsConfig.Repo.EnsureUpdated()
		if err != nil {
			ec.Logger.Debugf("cannot update plugin index %v", err)
		}
		version = ec.Version.CLISemver
	}
	plugin, err := ec.PluginsConfig.GetPlugin(name, plugins.FetchOpts{
		Version: version,
	})
	if err != nil {
		if err != plugins.ErrIsAlreadyInstalled {
			return errors.Wrapf(err, "cannot fetch plugin manfiest %s", name)
		}
		return nil
	}
	if ec.Spinner.Active() {
		prevPrefix := ec.Spinner.Prefix
		defer ec.Spin(prevPrefix)
	}
	ec.Spin(fmt.Sprintf("Installing plugin %s...", name))
	defer ec.Spinner.Stop()
	err = ec.PluginsConfig.Install(plugin)
	if err != nil {
		msg := fmt.Sprintf(`unable to install %s plugin. execute the following commands to continue:

  hasura plugins install %s
`, name, name)
		ec.Logger.Info(msg)
		return errors.Wrapf(err, "cannot install plugin %s", name)
	}
	ec.Logger.WithField("name", name).Infoln("plugin installed")
	return nil
}

func GetAdminSecretHeaderName(v *version.Version) string {
	if v.ServerFeatureFlags.HasAccessKey {
		return XHasuraAccessKey
	}
	return XHasuraAdminSecret
}
