package commands

import (
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/fsm"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func NewDeployCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &DeployOptions{
		EC: ec,
	}
	deployCmd := &cobra.Command{
		Use:   "deploy",
		Short: "(PREVIEW) Utility command to apply metadata & database migrations to graphql-engine",
		Example: `  
  # Apply metadata and migrations on Hasura GraphQL engine
  hasura deploy

  # Use with admin secret:
  hasura deploy --admin-secret "<admin-secret>"

  # Use with endpoint:
  hasura deploy --endpoint "<endpoint>"`,
		SilenceUsage: false,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			if err := ec.Validate(); err != nil {
				return err
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.Run()
		},
	}

	f := deployCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL engine")
	f.String("access-key", "", "access key for Hasura GraphQL engine")
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	f.Bool("insecure-skip-tls-verify", false, "skip TLS verification and disable cert checking (default: false)")
	f.String("certificate-authority", "", "path to a cert file for the certificate authority")
	f.Bool("disable-interactive", false, "disables interactive prompts (default: false)")

	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))
	util.BindPFlag(v, "disable_interactive", f.Lookup("disable-interactive"))

	f.BoolVar(&ec.DisableAutoStateMigration, "disable-auto-state-migration", false, "after a config v3 update, disable automatically moving state from hdb_catalog.schema_migrations to catalog state")
	if err := f.MarkHidden("disable-auto-state-migration"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	return deployCmd
}

type DeployOptions struct {
	EC *cli.ExecutionContext
}

func (opts *DeployOptions) Run() error {
	opts.EC.Config.DisableInteractive = true

	context := &deployCtx{
		ec:     opts.EC,
		logger: opts.EC.Logger,
		err:    nil,
	}

	if opts.EC.Config.Version <= cli.V2 {
		configV2FSM := newConfigV2DeployFSM()
		if err := configV2FSM.SendEvent(applyMigrations, context); err != nil {
			return err
		}
		if configV2FSM.Current == failedOperation {
			return fmt.Errorf("operation failed: %w", context.err)
		}
		return nil
	}

	configV3FSM := newConfigV3DeployFSM()
	if err := configV3FSM.SendEvent(applyInitialMetadata, context); err != nil {
		return err
	}
	if configV3FSM.Current == failedOperation {
		return fmt.Errorf("operation failed: %w", context.err)
	}
	return nil
}

type stateType = fsm.StateType

const (
	applyingInitialMetadata       stateType = "Applying Initial Metadata"
	applyingInitialMetadataFailed stateType = "Applying Initial Metadata Failed"
	applyingMetadata              stateType = "Applying Metadata"
	applyingMetadataFailed        stateType = "Applying Metadata Failed"
	applyingMigrations            stateType = "Applying Migrations"
	applyingMigrationsFailed      stateType = "Applying Migrations Failed"
	reloadingMetadata             stateType = "Reloading Metadata"
	reloadingMetadataFailed       stateType = "Reloading Metadata Failed"
	failedOperation               stateType = "Operation Failed"
)

type eventType = fsm.EventType

const (
	applyInitialMetadata       eventType = "Apply Initial Metadata"
	applyInitialMetadataFailed eventType = "Apply Initial Metadata Failed"
	applyMetadata              eventType = "Apply Metadata"
	applyMetadataFailed        eventType = "Apply Metadata Failed"
	applyMigrations            eventType = "Apply Migrations"
	applyMigrationsFailed      eventType = "Apply Migrations Failed"
	reloadMetadata             eventType = "Reload Metadata"
	reloadMetadataFailed       eventType = "Reload Metadata Failed"
	failOperation              eventType = "Operation Failed"
)

type deployCtx struct {
	ec     *cli.ExecutionContext
	logger *logrus.Logger
	err    error
}

type applyingInitialMetadataAction struct{}

func (a *applyingInitialMetadataAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	opts := MetadataApplyOptions{
		EC: context.ec,
	}
	context.logger.Debug(applyingInitialMetadata)
	if err := opts.Run(); err != nil {
		context.err = err
		return applyInitialMetadataFailed
	}
	return applyMigrations
}

type applyingInitialMetadataFailedAction struct{}

func (a *applyingInitialMetadataFailedAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(applyingInitialMetadataFailed)
	if context.err != nil {
		context.logger.Errorf("applying metadata failed")
		context.logger.Info("This can happen when metadata in your project metadata directory is malformed")
	}
	return failOperation
}

type applyingMigrationsAction struct{}

func (a *applyingMigrationsAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(applyingMigrations)
	disableInteractive := context.ec.Config.DisableInteractive
	defer func() { context.ec.Config.DisableInteractive = disableInteractive }()

	context.ec.Config.DisableInteractive = true
	opts := MigrateApplyOptions{
		EC: context.ec,
	}
	opts.EC.AllDatabases = true
	if err := opts.Run(); err != nil {
		context.err = err
		return applyMigrationsFailed
	}
	return applyMetadata
}

type applyingMigrationsFailedAction struct{}

func (a *applyingMigrationsFailedAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(applyingMigrationsFailed)
	if context.err != nil {
		context.logger.Errorf("applying migrations failed")
	}
	return failOperation
}

type applyingMetadataAction struct{}

func (a *applyingMetadataAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(applyingMetadata)
	opts := MetadataApplyOptions{
		EC: context.ec,
	}
	opts.EC.Spin("Applying metadata...")
	if err := opts.Run(); err != nil {
		opts.EC.Spinner.Stop()
		context.err = err
		return applyMetadataFailed
	}
	opts.EC.Spinner.Stop()
	opts.EC.Logger.Info("Metadata applied")
	return reloadMetadata
}

type applyingMetadataFailedAction struct{}

func (a *applyingMetadataFailedAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(applyingMetadataFailed)
	if context.err != nil {
		context.logger.Errorf("applying metadata failed")
	}
	return failOperation
}

type reloadingMetadataAction struct{}

func (a *reloadingMetadataAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(reloadingMetadata)
	opts := MetadataReloadOptions{
		EC: context.ec,
	}
	if err := opts.runWithInfo(); err != nil {
		context.err = err
		return reloadMetadataFailed
	}
	return fsm.NoOp
}

type reloadingMetadataFailedAction struct{}

func (a *reloadingMetadataFailedAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(reloadingMetadataFailed)
	if context.err != nil {
		context.logger.Errorf("reloading metadata failed")
	}
	return failOperation
}

type failedOperationAction struct{}

func (a *failedOperationAction) Execute(ctx fsm.EventContext) eventType {
	context := ctx.(*deployCtx)
	context.logger.Debug(failedOperation)
	return fsm.NoOp
}

func newConfigV3DeployFSM() *fsm.StateMachine {
	type State = fsm.State
	type States = fsm.States
	type Events = fsm.Events
	return &fsm.StateMachine{
		States: States{
			fsm.Default: State{
				Events: Events{
					applyInitialMetadata: applyingInitialMetadata,
				},
			},
			applyingInitialMetadata: State{
				Action: &applyingInitialMetadataAction{},
				Events: Events{
					applyInitialMetadataFailed: applyingInitialMetadataFailed,
					applyMigrations:            applyingMigrations,
				},
			},
			applyingInitialMetadataFailed: State{
				Action: &applyingInitialMetadataFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			applyingMigrations: State{
				Action: &applyingMigrationsAction{},
				Events: Events{
					applyMigrationsFailed: applyingMigrationsFailed,
					applyMetadata:         applyingMetadata,
				},
			},
			applyingMigrationsFailed: State{
				Action: &applyingMigrationsFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			applyingMetadata: State{
				Action: &applyingMetadataAction{},
				Events: Events{
					applyMetadataFailed: applyingMetadataFailed,
					reloadMetadata:      reloadingMetadata,
				},
			},
			applyingMetadataFailed: State{
				Action: &applyingMetadataFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			reloadingMetadata: State{
				Action: &reloadingMetadataAction{},
				Events: Events{
					reloadMetadataFailed: reloadingMetadataFailed,
				},
			},
			reloadingMetadataFailed: State{
				Action: &reloadingMetadataFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			failedOperation: State{
				Action: &failedOperationAction{},
			},
		},
	}
}

func newConfigV2DeployFSM() *fsm.StateMachine {
	type State = fsm.State
	type States = fsm.States
	type Events = fsm.Events
	return &fsm.StateMachine{
		States: States{
			fsm.Default: State{
				Events: Events{
					applyMigrations: applyingMigrations,
					failOperation:   failedOperation,
				},
			},
			applyingMigrations: State{
				Action: &applyingMigrationsAction{},
				Events: Events{
					applyMigrationsFailed: applyingMigrationsFailed,
					applyMetadata:         applyingMetadata,
				},
			},
			applyingMigrationsFailed: State{
				Action: &applyingMigrationsFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			applyingMetadata: State{
				Action: &applyingMetadataAction{},
				Events: Events{
					applyMetadataFailed: applyingMetadataFailed,
					reloadMetadata:      reloadingMetadata,
				},
			},
			applyingMetadataFailed: State{
				Action: &applyingMetadataFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			reloadingMetadata: State{
				Action: &reloadingMetadataAction{},
				Events: Events{
					reloadMetadataFailed: reloadingMetadataFailed,
				},
			},
			reloadingMetadataFailed: State{
				Action: &reloadingMetadataFailedAction{},
				Events: Events{
					failOperation: failedOperation,
				},
			},
			failedOperation: State{
				Action: &failedOperationAction{},
			},
		},
	}
}
