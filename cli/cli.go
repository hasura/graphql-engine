// Package cli and it's sub packages implements the command line tool for Hasura
// GraphQL Engine. The CLI operates on a directory, denoted by
// "ExecutionDirectory" in the "ExecutionContext" struct.
//
// The ExecutionContext is passed to all the subcommands so that a singleton
// context is available for the execution. Logger and Spinner comes from the same
// context.
package cli

import (
	"bytes"
	"context"
	"encoding/json"
	stderrors "errors"
	"fmt"
	"io"
	"io/fs"
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

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/pgdump"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1graphql"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1version"
	"github.com/hasura/graphql-engine/cli/v2/migrate/database/hasuradb"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1metadata"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1query"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v2query"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/commonmetadata"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore/settings"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore/migrations"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/Masterminds/semver"
	"github.com/briandowns/spinner"
	"github.com/cockroachdb/redact"
	"github.com/gofrs/uuid"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/v2/plugins"
	"github.com/hasura/graphql-engine/cli/v2/telemetry"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
	"github.com/subosito/gotenv"
	"golang.org/x/term"
	"gopkg.in/yaml.v3"
)

// Other constants used in the package
const (
	// Name of the global configuration directory
	GlobalConfigDirName = ".hasura"
	// Name of the global configuration file
	GlobalConfigFileName = "config.json"

	// Name of the file to store last update check time
	LastUpdateCheckFileName = "last_update_check_at"

	DefaultMigrationsDirectory = "migrations"
	DefaultMetadataDirectory   = "metadata"
	DefaultSeedsDirectory      = "seeds"
)

const (
	XHasuraAdminSecret = "X-Hasura-Admin-Secret"
	XHasuraAccessKey   = "X-Hasura-Access-Key"
)

const (
	TelemetryNotice = `Help us improve Hasura! The cli collects anonymized usage stats which
allow us to keep improving Hasura at warp speed. To opt-out or read more,
visit https://hasura.io/docs/latest/graphql/core/guides/telemetry.html
`
)

// ConfigVersion defines the version of the Config.
type ConfigVersion int

const (
	// V1 represents config version 1
	V1 ConfigVersion = iota + 1
	// V2 represents config version 2
	V2
	V3
)

type MetadataMode int

const (
	MetadataModeDirectory MetadataMode = iota
	MetadataModeJSON
	MetadataModeYAML
)

// ServerAPIPaths has the custom paths defined for server api
type ServerAPIPaths struct {
	V1Query    string `yaml:"v1_query,omitempty"`
	V2Query    string `yaml:"v2_query,omitempty"`
	V1Metadata string `yaml:"v1_metadata,omitempty"`
	GraphQL    string `yaml:"graphql,omitempty"`
	Config     string `yaml:"config,omitempty"`
	PGDump     string `yaml:"pg_dump,omitempty"`
	Version    string `yaml:"version,omitempty"`
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
	var op errors.Op = "cli.ConfigVersion.Set"
	v, err := strconv.ParseInt(s, 0, 64)
	*c = ConfigVersion(v)
	if err != nil {
		return errors.E(op, err)
	}
	if !c.IsValid() {
		return errors.E(op, ErrInvalidConfigVersion)
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
	return c != 0 && c <= V3
}

// ServerConfig has the config values required to contact the server
type ServerConfig struct {
	// Endpoint for the GraphQL Engine
	Endpoint string `yaml:"endpoint"`
	// AccessKey (deprecated) (optional) Admin secret key required to query the endpoint
	AccessKey string `yaml:"access_key,omitempty"`
	// AdminSecret (optional) Admin secret required to query the endpoint
	AdminSecret string `yaml:"admin_secret,omitempty"`
	// Config option to allow specifying multiple admin secrets
	// https://hasura.io/docs/latest/graphql/cloud/security/multiple-admin-secrets/
	AdminSecrets []string `yaml:"admin_secrets,omitempty"`
	// APIPaths (optional) API paths for server
	APIPaths *ServerAPIPaths `yaml:"api_paths,omitempty"`
	// InsecureSkipTLSVerify - indicates if TLS verification is disabled or not.
	InsecureSkipTLSVerify bool `yaml:"insecure_skip_tls_verify,omitempty"`
	// CAPath - Path to a cert file for the certificate authority
	CAPath string `yaml:"certificate_authority,omitempty"`

	ParsedEndpoint *url.URL `yaml:"-"`

	HTTPClient                 *httpc.Client              `yaml:"-"`
	HasuraServerInternalConfig HasuraServerInternalConfig `yaml:"-"`
}

func (c *ServerConfig) GetAdminSecret() string {
	// when HGE is configured with an admin secret, all API requests to HGE should be
	// authenticated using a x-hasura-admin-secret header.
	// admin secrets can be configured with two environment variables
	// 	- HASURA_GRAPHQL_ADMIN_SECRET (ref: https://hasura.io/docs/latest/graphql/core/deployment/deployment-guides/docker/#docker-secure)
	// 	- HASURA_GRAPHQL_ADMIN_SECRETS (ref: https://hasura.io/docs/latest/graphql/cloud/security/multiple-admin-secrets/)
	// the environment variable HASURA_GRAPHQL_ADMIN_SECRETS takes precedence when set
	if len(c.AdminSecrets) > 0 {
		// when HASURA_GRAPHQL_ADMIN_SECRETS environment variable is set, use the first available admin secret as the value of the header
		return c.AdminSecrets[0]
	} else if c.AdminSecret != "" {
		return c.AdminSecret
	}
	return ""
}

func (c *ServerConfig) GetHasuraInternalServerConfig(client *httpc.Client) error {
	var op errors.Op = "cli.ServerConfig.GetHasuraInternalServerConfig"
	// Determine from where assets should be served
	url := c.getConfigEndpoint()
	ctx, cancelFunc := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancelFunc()
	req, err := client.NewRequest("GET", url, nil)
	if err != nil {
		return errors.E(op, fmt.Errorf("error fetching config from server: %w", err))
	}
	r, err := client.Do(ctx, req, &c.HasuraServerInternalConfig)
	if err != nil {
		return errors.E(op, errors.KindNetwork, err)
	}
	defer r.Body.Close()

	if r.StatusCode != http.StatusOK {
		var horror hasuradb.HasuraError
		err := json.NewDecoder(r.Body).Decode(&horror)
		if err != nil {
			return errors.E(op, errors.KindHasuraAPI, fmt.Errorf("error unmarshalling fetching server config"))
		}
		return errors.E(op, errors.KindHasuraAPI, fmt.Errorf("error fetching server config: %v", horror.Error()))
	}
	return nil
}

// HasuraServerConfig is the type returned by the v1alpha1/config API
// TODO: Move this type to a client implementation for hasura
type HasuraServerInternalConfig struct {
	ConsoleAssetsDir string `json:"console_assets_dir"`
}

// GetVersionEndpoint provides the url to contact the version API
func (c *ServerConfig) GetVersionEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.Version)
	return nurl.String()
}

// GetQueryEndpoint provides the url to contact the query API
func (c *ServerConfig) GetV1QueryEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.V1Query)
	return nurl.String()
}

func (c *ServerConfig) GetV2QueryEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.V2Query)
	return nurl.String()
}

func (c *ServerConfig) GetPGDumpEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.PGDump)
	return nurl.String()
}

func (c *ServerConfig) GetV1GraphqlEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.GraphQL)
	return nurl.String()
}

// GetQueryEndpoint provides the url to contact the query API
func (c *ServerConfig) GetV1MetadataEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.V1Metadata)
	return nurl.String()
}

// GetVersionEndpoint provides the url to contact the config API
func (c *ServerConfig) getConfigEndpoint() string {
	nurl := *c.ParsedEndpoint
	nurl.Path = path.Join(nurl.Path, c.APIPaths.Config)
	return nurl.String()
}

// ParseEndpoint ensures the endpoint is valid.
func (c *ServerConfig) ParseEndpoint() error {
	var op errors.Op = "cli.ServerConfig.ParseEndpoint"
	nurl, err := url.ParseRequestURI(c.Endpoint)
	if err != nil {
		return errors.E(op, err)
	}
	c.ParsedEndpoint = nurl
	return nil
}

// Config represents configuration required for the CLI to function
type Config struct {
	// Version of the config.
	Version ConfigVersion `yaml:"version,omitempty"`

	// DisableInteractive disables interactive prompt
	DisableInteractive bool `yaml:"disable_interactive,omitempty"`

	// ServerConfig to be used by CLI to contact server.
	ServerConfig `yaml:",inline"`

	// MetadataDirectory defines the directory where the metadata files were stored.
	MetadataDirectory string `yaml:"metadata_directory,omitempty"`
	// MetadataFile defines the path in which a JSON/YAML metadata file should be stored
	MetadataFile string `yaml:"metadata_file,omitempty"`

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
	CMDName        string
	Stderr, Stdout io.Writer

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
	// MetadataFile is the name of json/yaml file where metadata will be stored
	MetadataFile string

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

	// instance of API client which communicates with Hasura API
	APIClient *hasura.Client

	// current database on which operation is being done
	Source        Source
	HasMetadataV3 bool

	// AllDatabases should be taken only incase if database isn't mentioned
	AllDatabases bool

	// after a `scripts update-config-v3` all migrate commands will try to automatically
	// move cli state from hdb_catalog.* tables to catalog state if that hasn't happened
	// already this configuration option will disable this step
	// more details in: https://github.com/hasura/graphql-engine/issues/6861
	DisableAutoStateMigration bool

	// CliExtDestinationDir is the directory path that will be used to setup cli-ext
	CliExtDestinationDir string

	// CliExtDestinationBinPath is the full path of the cli-ext binary
	CliExtDestinationBinPath string

	// CLIExtSourceBinPath is the full path to a copy of cli-ext binary in the local file system
	CliExtSourceBinPath string

	// proPluginVersionValidated is used to avoid validating pro plugin multiple times
	// while preparing the execution context
	proPluginVersionValidated bool

	MetadataMode MetadataMode

	// Any request headers that has to be sent with every HTTP request that CLI sends to HGE
	requestHeaders map[string]string
}

func (ec *ExecutionContext) AddRequestHeaders(headers map[string]string) {
	if ec.requestHeaders == nil {
		ec.requestHeaders = map[string]string{}
	}
	for k, v := range headers {
		ec.requestHeaders[k] = v
	}
}

type Source struct {
	Name string
	Kind hasura.SourceKind
}

// NewExecutionContext returns a new instance of execution context
func NewExecutionContext() *ExecutionContext {
	ec := &ExecutionContext{
		Stderr: os.Stderr,
		Stdout: os.Stdout,
	}
	ec.MetadataMode = MetadataModeDirectory
	ec.Telemetry = telemetry.BuildEvent()
	ec.Telemetry.Version = version.BuildVersion
	return ec
}

// Prepare as the name suggests, prepares the ExecutionContext ec by
// initializing most of the variables to sensible defaults, if it is not already
// set.
func (ec *ExecutionContext) Prepare() error {
	var op errors.Op = "cli.ExecutionContext.Prepare"
	// set the command name
	cmdName := os.Args[0]
	if len(cmdName) == 0 {
		cmdName = "hasura"
	}
	ec.CMDName = cmdName

	ec.IsTerminal = term.IsTerminal(int(os.Stdout.Fd()))

	// set spinner
	ec.setupSpinner()

	// set logger
	ec.setupLogger()

	// populate version
	ec.setVersion()

	// setup global config
	err := ec.setupGlobalConfig()
	if err != nil {
		return errors.E(op, fmt.Errorf("setting up global config failed: %w", err))
	}

	if !ec.proPluginVersionValidated {
		ec.validateProPluginVersion()
		ec.proPluginVersionValidated = true
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

// SetupPlugins create and returns the inferred paths for hasura. By default, it assumes
// $HOME/.hasura as the base path
func (ec *ExecutionContext) SetupPlugins() error {
	var op errors.Op = "cli.ExecutionContext.SetupPlugins"
	base := filepath.Join(ec.GlobalConfigDir, "plugins")
	base, err := filepath.Abs(base)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot get absolute path: %w", err))
	}
	ec.PluginsConfig = plugins.New(base)
	ec.PluginsConfig.Logger = ec.Logger
	ec.PluginsConfig.Repo.Logger = ec.Logger
	if ec.GlobalConfig.CLIEnvironment == ServerOnDockerEnvironment {
		ec.PluginsConfig.Repo.DisableCloneOrUpdate = true
	}
	err = ec.PluginsConfig.Prepare()
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (ec *ExecutionContext) validateProPluginVersion() {
	if err := ec.SetupPlugins(); err != nil {
		ec.Logger.Debugf("Validating installed pro plugin version failed: %v", err)
		return
	}
	installedPlugins, err := ec.PluginsConfig.ListInstalledPlugins()
	if err != nil {
		return
	}

	proPluginVersion := installedPlugins["pro"]
	cliVersion := ec.Version.GetCLIVersion()

	proPluginSemVer, _ := semver.NewVersion(proPluginVersion)
	cliSemVer := ec.Version.CLISemver
	if proPluginSemVer == nil || cliSemVer == nil {
		return
	}

	if cliSemVer.Major() != proPluginSemVer.Major() {
		ec.Logger.Warnf("[cli: %s] [pro plugin: %s] incompatible version of cli and pro plugin.", cliVersion, proPluginVersion)
		ec.Logger.Warn("Try running `hasura plugins upgrade pro` or `hasura plugins install pro --version <version>`")
	}
}

func (ec *ExecutionContext) SetupCodegenAssetsRepo() error {
	var op errors.Op = "cli.ExecutionContext.SetupCodegenAssetsRepo"
	base := filepath.Join(ec.GlobalConfigDir, util.ActionsCodegenDirName)
	base, err := filepath.Abs(base)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot get absolute path: %w", err))
	}
	ec.CodegenAssetsRepo = util.NewGitUtil(util.ActionsCodegenRepoURI, base, "")
	ec.CodegenAssetsRepo.Logger = ec.Logger
	if ec.GlobalConfig.CLIEnvironment == ServerOnDockerEnvironment {
		ec.CodegenAssetsRepo.DisableCloneOrUpdate = true
	}
	return nil
}

// Validate prepares the ExecutionContext ec and then validates the
// ExecutionDirectory to see if all the required files and directories are in
// place.
func (ec *ExecutionContext) Validate() error {
	var op errors.Op = "cli.ExecutionContext.Validate"
	// validate execution directory
	err := ec.validateDirectory()
	if err != nil {
		return errors.E(op, fmt.Errorf("validating current directory failed: %w", err))
	}

	// load .env file
	err = ec.loadEnvfile()
	if err != nil {
		return errors.E(op, fmt.Errorf("loading .env file failed: %w", err))
	}

	// set names of config file
	ec.ConfigFile = filepath.Join(ec.ExecutionDirectory, "config.yaml")

	// read config and parse the values into Config
	err = ec.readConfig()
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot read config: %w", err))
	}

	// initialize HTTP client
	// CLI uses a common http client defined in internal/httpc.Client
	// get TLS Config
	tlsConfig, err := httpc.GenerateTLSConfig(ec.Config.CAPath, ec.Config.InsecureSkipTLSVerify)
	if err != nil || tlsConfig == nil {
		return errors.E(op, fmt.Errorf("error while getting TLS config"))
	}

	// create a net/http.Client with TLS Config
	standardHttpClient, err := httpc.NewHttpClientWithTLSConfig(tlsConfig)
	if err != nil || standardHttpClient == nil {
		return errors.E(op, fmt.Errorf("error while creating http client with TLS configuration %w", err))
	}

	// create httpc.Client
	httpClient, err := httpc.New(standardHttpClient, ec.Config.Endpoint, ec.HGEHeaders)
	if err != nil || httpClient == nil {
		return errors.E(op, err)
	}
	ec.Config.HTTPClient = httpClient

	err = util.GetServerStatus(ec.Config.GetVersionEndpoint(), ec.Config.HTTPClient)
	if err != nil {
		ec.Logger.Error("connecting to graphql-engine server failed")
		ec.Logger.Info("possible reasons:")
		ec.Logger.Info("1) Provided root endpoint of graphql-engine server is wrong. Verify endpoint key in config.yaml or/and value of --endpoint flag")
		ec.Logger.Info("2) Endpoint should NOT be your GraphQL API, ie endpoint is NOT https://hasura-cloud-app.io/v1/graphql it should be: https://hasura-cloud-app.io")
		ec.Logger.Info("3) Server might be unhealthy and is not running/accepting API requests")
		ec.Logger.Info("4) Admin secret is not correct/set")
		ec.Logger.Infoln()
		return errors.E(op, err)
	}

	// get version from the server and match with the cli version
	err = ec.checkServerVersion()
	if err != nil {
		return errors.E(op, fmt.Errorf("version check: %w", err))
	}

	// get the server feature flags
	err = ec.Version.GetServerFeatureFlags()
	if err != nil {
		return errors.E(op, fmt.Errorf("error in getting server feature flags %w", err))
	}

	ec.AddRequestHeaders(map[string]string{GetAdminSecretHeaderName(ec.Version): ec.Config.GetAdminSecret()})

	ec.Config.HTTPClient.SetHeaders(ec.requestHeaders)

	// this populates the ec.Config.ServerConfig.HasuraServerInternalConfig
	err = ec.Config.ServerConfig.GetHasuraInternalServerConfig(httpClient)
	if err != nil {
		// If config API is not enabled log it and don't fail
		ec.Logger.Debugf("cannot get config information from server, this might be because config API is not enabled: %v", err)
	}

	// set name of migration directory
	ec.MigrationDir = filepath.Join(ec.ExecutionDirectory, ec.Config.MigrationsDirectory)
	if _, err := os.Stat(ec.MigrationDir); stderrors.Is(err, fs.ErrNotExist) {
		err = os.MkdirAll(ec.MigrationDir, os.ModePerm)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot create migrations directory: %w", err))
		}
	}

	ec.SeedsDirectory = filepath.Join(ec.ExecutionDirectory, ec.Config.SeedsDirectory)
	if _, err := os.Stat(ec.SeedsDirectory); stderrors.Is(err, fs.ErrNotExist) {
		err = os.MkdirAll(ec.SeedsDirectory, os.ModePerm)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot create seeds directory: %w", err))
		}
	}

	if ec.Config.Version >= V2 && ec.Config.MetadataDirectory != "" {
		if len(ec.Config.MetadataFile) > 0 {
			ec.MetadataFile = filepath.Join(ec.ExecutionDirectory, ec.Config.MetadataFile)
			if _, err := os.Stat(ec.MetadataFile); stderrors.Is(err, fs.ErrNotExist) {
				if err := ioutil.WriteFile(ec.MetadataFile, []byte(""), os.ModePerm); err != nil {
					return errors.E(op, err)
				}
			}
			switch filepath.Ext(ec.MetadataFile) {
			case ".json":
				ec.MetadataMode = MetadataModeJSON
			case ".yaml":
				ec.MetadataMode = MetadataModeYAML
			default:
				return errors.E(op, fmt.Errorf("unrecogonized file extension. only .json/.yaml files are allowed for value of metadata_file"))
			}
		}
		// set name of metadata directory
		ec.MetadataDir = filepath.Join(ec.ExecutionDirectory, ec.Config.MetadataDirectory)
		if _, err := os.Stat(ec.MetadataDir); stderrors.Is(err, fs.ErrNotExist) && !(len(ec.MetadataFile) > 0) {
			err = os.MkdirAll(ec.MetadataDir, os.ModePerm)
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot create metadata directory: %w", err))
			}
		}
	}

	ec.Logger.Debug("graphql engine endpoint: ", ec.Config.ServerConfig.Endpoint)
	ec.Logger.Debug(redact.Sprintf("graphql engine admin_secret: %s", ec.Config.ServerConfig.AdminSecret).Redact())

	uri, err := url.Parse(ec.Config.Endpoint)
	if err != nil {
		return errors.E(op, fmt.Errorf("error while parsing the endpoint :%w", err))
	}

	// check if server is using metadata v3
	if ec.Config.APIPaths.V1Query != "" {
		uri.Path = path.Join(uri.Path, ec.Config.APIPaths.V1Query)
	} else {
		uri.Path = path.Join(uri.Path, "v1/query")
	}
	requestUri := uri.String()
	metadata, err := commonmetadata.New(httpClient, requestUri).ExportMetadata()
	if err != nil {
		return errors.E(op, err)
	}
	var v struct {
		Version int `json:"version"`
	}
	if err := json.NewDecoder(metadata).Decode(&v); err != nil {
		return errors.E(op, err)
	}
	if v.Version == 3 {
		ec.HasMetadataV3 = true
	}
	if ec.Config.Version >= V3 && !ec.HasMetadataV3 {
		return errors.E(op, fmt.Errorf(`config v3 can only be used with servers having metadata version >= 3
You could fix this problem by taking one of the following actions:
1. Upgrade your Hasura server to a newer version (>= v2.0.0) ie upgrade to a version which supports metadata v3
2. Force CLI to use an older config version via the --version <VERSION> flag`))
	}

	ec.APIClient = &hasura.Client{
		V1Metadata: v1metadata.New(httpClient, ec.Config.GetV1MetadataEndpoint()),
		V1Query:    v1query.New(httpClient, ec.Config.GetV1QueryEndpoint()),
		V2Query:    v2query.New(httpClient, ec.Config.GetV2QueryEndpoint()),
		PGDump:     pgdump.New(httpClient, ec.Config.GetPGDumpEndpoint()),
		V1Graphql:  v1graphql.New(httpClient, ec.Config.GetV1GraphqlEndpoint()),
		V1Version:  v1version.New(httpClient, ec.Config.GetVersionEndpoint()),
	}
	var state *util.ServerState
	if ec.HasMetadataV3 {
		state = util.GetServerState(httpClient, ec.Config.GetV1MetadataEndpoint(), ec.HasMetadataV3, ec.Logger)
	} else {
		state = util.GetServerState(httpClient, ec.Config.GetV1QueryEndpoint(), ec.HasMetadataV3, ec.Logger)
	}
	ec.ServerUUID = state.UUID
	ec.Telemetry.ServerUUID = ec.ServerUUID
	ec.Logger.Debugf("server: uuid: %s", ec.ServerUUID)
	// Set headers required for communicating with HGE
	return nil
}

func (ec *ExecutionContext) checkServerVersion() error {
	var op errors.Op = "cli.ExecutionContext.checkServerVersion"
	v, err := version.FetchServerVersion(ec.Config.ServerConfig.GetVersionEndpoint(), ec.Config.HTTPClient)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to get version from server: %w", err))
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
	var op errors.Op = "cli.ExecutionContext.WriteConfig"
	var cfg *Config
	if config != nil {
		cfg = config
	} else {
		cfg = ec.Config
	}
	buf := new(bytes.Buffer)
	encoder := yaml.NewEncoder(buf)
	encoder.SetIndent(2)
	err := encoder.Encode(cfg)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(ec.ConfigFile, buf.Bytes(), 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

type DefaultAPIPath string

// readConfig reads the configuration from config file, flags and env vars,
// through viper.
func (ec *ExecutionContext) readConfig() error {
	var op errors.Op = "cli.ExecutionContext.readConfig"
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
	v.SetDefault("api_paths.v2_query", "v2/query")
	v.SetDefault("api_paths.v1_metadata", "v1/metadata")
	v.SetDefault("api_paths.graphql", "v1/graphql")
	v.SetDefault("api_paths.config", "v1alpha1/config")
	v.SetDefault("api_paths.pg_dump", "v1alpha1/pg_dump")
	v.SetDefault("api_paths.version", "v1/version")
	v.SetDefault("metadata_directory", DefaultMetadataDirectory)
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
		return errors.E(op, fmt.Errorf("cannot read config from file/env: %w", err))
	}
	adminSecret := v.GetString("admin_secret")
	if adminSecret == "" {
		adminSecret = v.GetString("access_key")
	}

	// Admin secrets can be specified as a string value of format
	// ["secret1", "secret2"], similar to how the corresponding environment variable
	// HASURA_GRAPHQL_ADMIN_SECRETS is configured with the server
	adminSecretsList := v.GetString("admin_secrets")
	adminSecrets := []string{}
	if len(adminSecretsList) > 0 {
		if err = json.Unmarshal([]byte(adminSecretsList), &adminSecrets); err != nil {
			return errors.E(op, fmt.Errorf("parsing 'admin_secrets' from config.yaml / environment variable HASURA_GRAPHQL_ADMIN_SECRETS failed: expected value of format [\"secret1\", \"secret2\"]: %w", err))
		}
	}

	ec.Config = &Config{
		Version:            ConfigVersion(v.GetInt("version")),
		DisableInteractive: v.GetBool("disable_interactive"),
		ServerConfig: ServerConfig{
			Endpoint:     v.GetString("endpoint"),
			AdminSecret:  adminSecret,
			AdminSecrets: adminSecrets,
			APIPaths: &ServerAPIPaths{
				V1Query:    v.GetString("api_paths.query"),
				V2Query:    v.GetString("api_paths.v2_query"),
				V1Metadata: v.GetString("api_paths.v1_metadata"),
				GraphQL:    v.GetString("api_paths.graphql"),
				Config:     v.GetString("api_paths.config"),
				PGDump:     v.GetString("api_paths.pg_dump"),
				Version:    v.GetString("api_paths.version"),
			},
			InsecureSkipTLSVerify: v.GetBool("insecure_skip_tls_verify"),
			CAPath:                v.GetString("certificate_authority"),
		},
		MetadataDirectory:   v.GetString("metadata_directory"),
		MetadataFile:        v.GetString("metadata_file"),
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
		return errors.E(op, ErrInvalidConfigVersion)
	}
	err = ec.Config.ServerConfig.ParseEndpoint()
	if err != nil {
		return errors.E(op, fmt.Errorf("unable to parse server endpoint: %w", err))
	}
	return nil
}

// setupSpinner creates a default spinner if the context does not already have
// one.
func (ec *ExecutionContext) setupSpinner() {
	if ec.Spinner == nil {
		spnr := spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		spnr.Writer = ec.Stderr
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
	var op errors.Op = "cli.ExecutionContext.loadEnvfile"
	envfile := ec.Envfile
	if !filepath.IsAbs(ec.Envfile) {
		envfile = filepath.Join(ec.ExecutionDirectory, ec.Envfile)
	}
	err := gotenv.Load(envfile)
	if err != nil {
		// return error if user provided envfile name
		if ec.Envfile != ".env" {
			return errors.E(op, err)
		}
		if !stderrors.Is(err, fs.ErrNotExist) {
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
		ec.Logger.SetOutput(ec.Stderr)
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

func GetAdminSecretHeaderName(v *version.Version) string {
	if v.ServerFeatureFlags.HasAccessKey {
		return XHasuraAccessKey
	}
	return XHasuraAdminSecret
}

func GetCommonMetadataOps(ec *ExecutionContext) hasura.CommonMetadataOperations {
	if !ec.HasMetadataV3 {
		return ec.APIClient.V1Query
	}
	return ec.APIClient.V1Metadata
}

func GetMigrationsStateStore(ec *ExecutionContext) statestore.MigrationsStateStore {
	if ec.Config.Version <= V2 {
		if !ec.HasMetadataV3 {
			return migrations.NewMigrationStateStoreHdbTable(ec.APIClient.V1Query, migrations.DefaultSchema, migrations.DefaultMigrationsTable)
		}
		return migrations.NewMigrationStateStoreHdbTable(ec.APIClient.V2Query, migrations.DefaultSchema, migrations.DefaultMigrationsTable)
	}
	return migrations.NewCatalogStateStore(statestore.NewCLICatalogState(ec.APIClient.V1Metadata))
}

func GetSettingsStateStore(ec *ExecutionContext, databaseName string) statestore.SettingsStateStore {
	const (
		defaultSettingsTable = "migration_settings"
		defaultSchema        = "hdb_catalog"
	)

	if ec.Config.Version <= V2 {
		if !ec.HasMetadataV3 {
			return settings.NewStateStoreHdbTable(ec.APIClient.V1Query, databaseName, defaultSchema, defaultSettingsTable)
		}
		return settings.NewStateStoreHdbTable(ec.APIClient.V2Query, databaseName, defaultSchema, defaultSettingsTable)
	}
	return settings.NewStateStoreCatalog(statestore.NewCLICatalogState(ec.APIClient.V1Metadata))
}
