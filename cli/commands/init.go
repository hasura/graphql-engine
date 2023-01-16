package commands

import (
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/hashicorp/go-multierror"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/fsm"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions"
	actionMetadataFileTypes "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/allowlist"
	crontriggers "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/cron_triggers"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/functions"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/querycollections"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/remoteschemas"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/sources"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/tables"
	metadataVersion "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/version"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const (
	defaultDirectory string = "hasura"
	defaultEndpoint  string = "http://localhost:8080"
)

// NewInitCmd is the definition for init command
func NewInitCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &InitOptions{
		EC: ec,
	}
	initCmd := &cobra.Command{
		Use:   "init [directory-name]",
		Short: "Initialize a new Hasura GraphQL Engine project",
		Long: `This is generally the first command that you would run in a new project. It creates a directory with the necessary files and directories to configure an instance of the Hasura GraphQL Engine. You can pass various flags to customize the behavior of the command and pre-configure environment variables.
		
Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/migrations-metadata-setup/
`,
		Example: `  # Create a directory to store migrations
  hasura init [directory-name]

  # Now, edit <my-directory>/config.yaml to add endpoint and admin secret

  # Create a directory with endpoint and admin secret configured:
  hasura init <my-project> --endpoint https://my-graphql-engine.com --admin-secret adminsecretkey

  # Create a Hasura Project in the current working directory
  hasura init .

  # See https://hasura.io/docs/latest/graphql/core/migrations/index.html for more details`,
		SilenceUsage: true,
		Args:         cobra.MaximumNArgs(1),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			ec.Viper = viper.New()
			err := ec.Prepare()
			if err != nil {
				return errors.E(op, err)
			}
			// show deprecation message if initializing a config v1 project
			if opts.Version <= cli.V1 {
				return errors.E(op, fmt.Errorf("config v1 is deprecated, please consider using config v3"))
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if len(args) == 1 {
				opts.InitDir = args[0]
			}
			if err := opts.Run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	f := initCmd.Flags()
	f.Var(cli.NewConfigVersionValue(cli.V3, &opts.Version), "version", "config version to be used")
	f.StringVar(&opts.InitDir, "directory", "", "name of directory where files will be created")
	f.StringVar(&opts.Endpoint, "endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.StringVar(&opts.AdminSecret, "access-key", "", "access key for Hasura GraphQL Engine")

	f.String("install-manifest", "", "install manifest to be cloned")
	if err := f.MarkDeprecated("install-manifest", "refer: https://github.com/hasura/graphql-engine/tree/stable/install-manifests"); err != nil {
		ec.Logger.Debugf("failed marking depricated flag")
	}
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	if err := f.MarkDeprecated("directory", "use directory-name argument instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	// only used in tests
	f.BoolVar(&opts.GetMetadataMigrations, "fetch", false, "It fetches the metadata and migrations from server without prompt")
	if err := f.MarkHidden("fetch"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	return initCmd
}

type InitOptions struct {
	EC *cli.ExecutionContext

	Version               cli.ConfigVersion
	Endpoint              string
	AdminSecret           string
	InitDir               string
	GetMetadataMigrations bool
}

func (o *InitOptions) InitRun() error {
	var op errors.Op = "commands.InitOptions.InitRun"
	// prompt for init directory if it's not set already
	if o.InitDir == "" {
		r, err := util.GetInputPromptWithDefault("Name of project directory ?", defaultDirectory)
		if err != nil {
			return errors.E(op, fmt.Errorf("prompt exited: %w", err))
		}
		if strings.TrimSpace(r) != "" {
			o.InitDir = r
		} else {
			o.InitDir = defaultDirectory
		}
	}
	if o.Endpoint != "" && !o.GetMetadataMigrations && o.EC.IsTerminal {
		r, err := util.GetYesNoPrompt(fmt.Sprintf("Initialize project with metadata & migrations from %s ?", o.Endpoint))
		if err != nil {
			return errors.E(op, fmt.Errorf("prompt exited: %w", err))
		}
		o.GetMetadataMigrations = r
	}
	if !o.EC.IsTerminal {
		o.GetMetadataMigrations = true
	}

	cwdir, err := os.Getwd()
	if err != nil {
		return errors.E(op, fmt.Errorf("error getting current working directory: %w", err))
	}
	initPath, err := filepath.Abs(o.InitDir)
	if err != nil {
		return errors.E(op, err)
	}
	if initPath == cwdir {
		// check if pwd is filesystem root
		if err := cli.CheckFilesystemBoundary(cwdir); err != nil {
			return errors.E(op, fmt.Errorf("can't initialise hasura project in filesystem root: %w", err))
		}
		// check if the current directory is already a hasura project
		if err := cli.ValidateDirectory(cwdir); err == nil {
			return errors.E(op, "current working directory is already a hasura project directory")
		}
		o.EC.ExecutionDirectory = cwdir
	} else {
		// create execution directory
		err := o.createExecutionDirectory()
		if err != nil {
			return errors.E(op, err)
		}
	}

	// create other required files, like config.yaml, migrations directory
	err = o.createFiles()
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

// create the execution directory
func (o *InitOptions) createExecutionDirectory() error {
	var op errors.Op = "commands.InitOptions.createExecutionDirectory"
	if o.EC.ExecutionDirectory == "" {
		o.EC.ExecutionDirectory = o.InitDir
	} else {
		o.EC.ExecutionDirectory = filepath.Join(o.EC.ExecutionDirectory, o.InitDir)
	}

	// create the execution directory
	if _, err := os.Stat(o.EC.ExecutionDirectory); err == nil {
		return errors.E(op, fmt.Errorf("directory '%s' already exists", o.EC.ExecutionDirectory))
	}
	err := os.MkdirAll(o.EC.ExecutionDirectory, os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("error creating setup directories: %w", err))
	}

	return nil
}

// createFiles creates files required by the CLI in the ExecutionDirectory
func (o *InitOptions) createFiles() error {
	var op errors.Op = "commands.InitOptions.createFiles"
	// create the directory
	err := os.MkdirAll(filepath.Dir(o.EC.ExecutionDirectory), os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("error creating setup directories: %w", err))
	}
	// set config object
	var config = &cli.Config{
		Version: o.Version,
		ServerConfig: cli.ServerConfig{
			Endpoint: defaultEndpoint,
		},
		MetadataDirectory: "metadata",
		ActionConfig: &actionMetadataFileTypes.ActionExecutionConfig{
			Kind:                  "synchronous",
			HandlerWebhookBaseURL: "http://localhost:3000",
		},
	}
	if o.Endpoint != "" {
		if _, err := url.ParseRequestURI(o.Endpoint); err != nil {
			return errors.E(op, fmt.Errorf("error validating endpoint URL: %w", err))
		}
		config.ServerConfig.Endpoint = o.Endpoint
	}
	if o.AdminSecret != "" {
		config.ServerConfig.AdminSecret = o.AdminSecret
	}

	// write the config file
	o.EC.Config = config
	o.EC.ConfigFile = filepath.Join(o.EC.ExecutionDirectory, "config.yaml")
	err = o.EC.WriteConfig(nil)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot write config file: %w", err))
	}

	// create migrations directory
	o.EC.MigrationDir = filepath.Join(o.EC.ExecutionDirectory, cli.DefaultMigrationsDirectory)
	err = os.MkdirAll(o.EC.MigrationDir, os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot write migration directory: %w", err))
	}

	if config.Version >= cli.V2 {
		// create metadata directory
		o.EC.MetadataDir = filepath.Join(o.EC.ExecutionDirectory, cli.DefaultMetadataDirectory)
		err = os.MkdirAll(o.EC.MetadataDir, os.ModePerm)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot write metadata directory: %w", err))
		}
		err = o.EC.Version.GetServerFeatureFlags()
		if err != nil {
			o.EC.Logger.Warnf("error determining server feature flags: %v", err)
		}

		// create metadata files
		plugins := make(metadataobject.Objects, 0)
		plugins = append(plugins, querycollections.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, allowlist.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, remoteschemas.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, actions.New(o.EC, o.EC.MetadataDir))
		plugins = append(plugins, crontriggers.New(o.EC, o.EC.MetadataDir))
		if config.Version == cli.V3 {
			plugins = append(plugins, metadataVersion.New(o.EC, o.EC.MetadataDir))
			plugins = append(plugins, sources.New(o.EC, o.EC.MetadataDir))
		} else {
			plugins = append(plugins, metadataVersion.NewV3MetadataVersion(o.EC, o.EC.MetadataDir))
			plugins = append(plugins, tables.New(o.EC, o.EC.MetadataDir))
			plugins = append(plugins, functions.New(o.EC, o.EC.MetadataDir))
		}

		for _, plg := range plugins {
			err := plg.CreateFiles()
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot create metadata files: %w", err))
			}
		}
	}

	// create seeds directory
	o.EC.SeedsDirectory = filepath.Join(o.EC.ExecutionDirectory, cli.DefaultSeedsDirectory)
	err = os.MkdirAll(o.EC.SeedsDirectory, os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot write seeds directory: %w", err))
	}
	return nil
}

func (o *InitOptions) Run() error {
	var op errors.Op = "commands.InitOptions.Run"
	context := &initCtx{
		ec:      o.EC,
		initOps: o,
		logger:  o.EC.Logger,
		err:     nil,
	}

	configInitFSM := newInitFSM()
	if err := configInitFSM.SendEvent(createProjectDirectory, context); err != nil {
		return errors.E(op, err)
	}
	if configInitFSM.Current == failedOperation {
		return errors.E(op, fmt.Errorf("operation failed: %w", context.err))
	}
	return nil
}

const (
	creatingProjectDirectory stateType = "Creating project directory"
	failedCreatingProjectDir stateType = "Failed to create project directory"
	validatingEndpoint       stateType = "Validating Endpoint"
	failedValidatingEndpoint stateType = "Failed validating endpoint"
	exportingMetadata        stateType = "Exporting Metadata"
	failedExportingMetadata  stateType = "Failed to export Metadata"
	creatingMigration        stateType = "Creating Migration"
	failedCreatingMigration  stateType = "Failed creating Migration"
	endState                 stateType = "End State"
)

const (
	createProjectDirectory       eventType = "Create project directory"
	createProjectDirectoryFailed eventType = "Create project directory failed"
	validateEndpoint             eventType = "Validate Endpoint"
	validateEndpointFailed       eventType = "Validate Endpoint Failed"
	exportMetadata               eventType = "Export Metadata"
	exportMetadataFailed         eventType = "Export Metadata Failed"
	createMigration              eventType = "Create migration from server"
	createMigrationFailed        eventType = "Create migration from server Failed"
	gotoEndstate                 eventType = "Go to End State"
)

type initCtx struct {
	ec      *cli.ExecutionContext
	initOps *InitOptions
	logger  *logrus.Logger
	err     error
}

type creatingDefaultDirAction struct{}

func (a *creatingDefaultDirAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	opts := context.initOps
	context.logger.Debug(creatingProjectDirectory)
	if err := opts.InitRun(); err != nil {
		context.err = err
		return createProjectDirectoryFailed
	}
	if len(opts.Endpoint) > 0 && opts.GetMetadataMigrations {
		return validateEndpoint
	}
	return gotoEndstate
}

type failedCreatingDefaultDirAction struct{}

func (a *failedCreatingDefaultDirAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	context.logger.Debug(failedCreatingProjectDir)
	if context.err != nil {
		context.logger.Errorln("initializing project directory failed")
	}
	return failOperation
}

type validatingEndpointAction struct{}

func (a *validatingEndpointAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	opts := context.initOps
	context.logger.Debug(validatingEndpoint)
	if err := opts.EC.Validate(); err != nil {
		context.err = err
		return validateEndpointFailed
	}
	if err := util.GetServerStatus(opts.EC.Config.GetVersionEndpoint(), opts.EC.Config.HTTPClient); err != nil {
		context.err = err
		return validateEndpointFailed
	}
	return exportMetadata
}

type failedValidatingEndpointAction struct{}

func (a *failedValidatingEndpointAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	context.logger.Debug(failedValidatingEndpoint)
	if context.err != nil {
		context.logger.Errorf("validating server failed: %v", context.err)
	}
	return gotoEndstate
}

type exportingMetadataAction struct{}

func (a *exportingMetadataAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	opts := MetadataExportOptions{
		EC: context.ec,
	}
	context.logger.Debug(exportingMetadata)
	if err := context.ec.Validate(); err != nil {
		context.err = err
		return exportMetadataFailed
	}
	// Note: Here ec won't use the values from `--endpoint` and `--admin-secret` or `--access-key` flags
	context.ec.Spin("Exporting metadata...")
	if err := opts.Run(); err != nil {
		opts.EC.Spinner.Stop()
		context.err = err
		return exportMetadataFailed
	}
	opts.EC.Spinner.Stop()
	opts.EC.Logger.Info("Metadata exported")
	return createMigration
}

type failedExportingMetadataAction struct{}

func (a *failedExportingMetadataAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	context.logger.Debug(failedExportingMetadata)
	if context.err != nil {
		context.logger.Errorf("exporting metadata failed: \n%v\n%s", context.err, "run `hasura metadata export` from your project directory to retry")
	}
	return createMigration
}

type creatingMigrationAction struct{}

func (a *creatingMigrationAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	opts := migrateCreateOptions{
		EC:             context.ec,
		name:           "init",
		fromServer:     true,
		excludeSchemas: []string{"hdb_catalog", "hdb_views"},
	}
	context.logger.Debug(creatingMigration)
	if err := context.ec.Validate(); err != nil {
		context.err = err
		return createMigrationFailed
	}
	// Note: Here ec won't use the values from `--endpoint` and `--admin-secret` or `--access-key` flags
	if opts.EC.Config.Version == cli.V2 {
		source := cli.Source{Name: "", Kind: hasura.SourceKindPG}
		opts.Source = source
		if _, err := opts.run(); err != nil {
			context.err = multierror.Append(context.err, err)
		}
	} else {
		sources, err := metadatautil.GetSourcesAndKind(opts.EC.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			context.err = err
			context.logger.Debugf("getting list of connected databases from server (%s) failed", context.initOps.Endpoint)
			return createMigrationFailed
		}
		for _, source := range sources {
			opts.EC.Logger.Infof("Creating migrations for source: %s", source.Name)
			opts.Source = cli.Source(source)
			if _, err := opts.run(); err != nil {
				context.err = multierror.Append(context.err, fmt.Errorf("applying migrations on source: %s: %w", source.Name, err))
			}
		}
	}
	if context.err != nil {
		return createMigrationFailed
	}

	return gotoEndstate
}

type failedCreatingMigrationAction struct{}

func (a *failedCreatingMigrationAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	context.logger.Debug(failedCreatingMigration)
	if context.err != nil {
		context.logger.Errorf("creating migrations failed: \n%v\n%s", context.err, "run `hasura migrate create --from-server` from your project directory to retry")
	}
	return gotoEndstate
}

type failedInitOperationAction struct{}

func (a *failedInitOperationAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	context.logger.Debug(failedOperation)
	return fsm.NoOp
}

type gotoEndstateAction struct{}

func (a *gotoEndstateAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*initCtx)
	opts := context.initOps
	cwdir, err := os.Getwd()
	if err != nil {
		context.logger.Errorf("error getting current working directory : %v", err)
		return fsm.NoOp
	}
	var infoMsg string
	if opts.EC.ExecutionDirectory != cwdir {
		infoMsg = fmt.Sprintf(`directory created. execute the following commands to continue:

  cd %s
  hasura console
`, opts.EC.ExecutionDirectory)
	} else {
		infoMsg = `hasura project initialised. execute the following command to continue:
		hasura console
	  `
	}
	context.logger.Infoln(infoMsg)
	return fsm.NoOp
}

func newInitFSM() *fsm.StateMachine {
	type State = fsm.State
	type States = fsm.States
	type Events = fsm.Events
	return &fsm.StateMachine{
		States: States{
			fsm.Default: State{
				Events: Events{
					createProjectDirectory: creatingProjectDirectory,
				},
			},
			creatingProjectDirectory: State{
				Action: &creatingDefaultDirAction{},
				Events: Events{
					createProjectDirectoryFailed: failedCreatingProjectDir,
					validateEndpoint:             validatingEndpoint,
					gotoEndstate:                 endState,
				},
			},
			failedCreatingProjectDir: State{
				Action: &failedCreatingDefaultDirAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			validatingEndpoint: State{
				Action: &validatingEndpointAction{},
				Events: Events{
					validateEndpointFailed: failedValidatingEndpoint,
					exportMetadata:         exportingMetadata,
				},
			},
			failedValidatingEndpoint: State{
				Action: &failedValidatingEndpointAction{},
				Events: Events{
					gotoEndstate: endState,
				},
			},
			exportingMetadata: State{
				Action: &exportingMetadataAction{},
				Events: Events{
					exportMetadataFailed: failedExportingMetadata,
					createMigration:      creatingMigration,
				},
			},
			failedExportingMetadata: State{
				Action: &failedExportingMetadataAction{},
				Events: Events{
					gotoEndstate:    endState,
					createMigration: creatingMigration,
				},
			},
			creatingMigration: State{
				Action: &creatingMigrationAction{},
				Events: Events{
					createMigrationFailed: failedCreatingMigration,
					gotoEndstate:          endState,
				},
			},
			failedCreatingMigration: State{
				Action: &failedCreatingMigrationAction{},
				Events: Events{
					gotoEndstate: endState,
				},
			},
			failedOperation: State{
				Action: &failedInitOperationAction{},
			},
			endState: State{
				Action: &gotoEndstateAction{},
				Events: Events{},
			},
		},
	}
}
