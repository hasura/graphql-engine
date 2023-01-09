package commands

import (
	"fmt"
	"path/filepath"
	"strconv"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	mig "github.com/hasura/graphql-engine/cli/v2/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/spf13/cobra"
)

func newMigrateDeleteCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MigrateDeleteOptions{
		EC: ec,
	}
	migrateDeleteCmd := &cobra.Command{
		Use:   "delete",
		Short: "(PREVIEW) clear migrations from local project and server",
		Long:  "This command deletes migrations from the local project and the server. You can delete all migrations or a specific migration version by passing in the appropriate flags and values.",
		Example: `
  # Usage to delete a version:
  hasura migrate delete --version <version_delete> --database-name <database-name>
  
  # Usage to delete all versions
   hasura migrate delete --all --database-name <database-name>`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			ec.Logger.Warn("[PREVIEW] this command is in preview. usage may change in future\n")
			if err := validateConfigV3FlagsWithAll(cmd, ec); err != nil {
				return errors.E(op, err)
			}
			if !cmd.Flags().Changed("all") && !cmd.Flags().Changed("version") {
				return errors.E(op, fmt.Errorf("at least one flag [--all , --version] should be set"))
			}
			if cmd.Flags().Changed("all") && cmd.Flags().Changed("version") {
				return errors.E(op, fmt.Errorf("only one of [--all , --version] should be set"))
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			// exit if user inputs n for clearing migrations
			if cmd.Flags().Changed("all") && !opts.Force && opts.EC.IsTerminal {
				confirmation, err := util.GetYesNoPrompt("clear all migrations of database and it's history on the server?")
				if err != nil {
					return errors.E(op, fmt.Errorf("error getting user input: %w", err))
				}
				if !confirmation {
					return nil
				}
			}

			if ec.AllDatabases && !opts.Force {
				confirmation, err := util.GetYesNoPrompt("clear all mentioned migrations of all databases and it's history on the server?")
				if err != nil {
					return errors.E(op, fmt.Errorf("error getting user input: %w", err))
				}
				if !confirmation {
					return nil
				}
			}

			if err := opts.Run(); err != nil {
				return errors.E(op, fmt.Errorf("operation failed: %w", err))
			}
			return nil
		},
	}

	f := migrateDeleteCmd.Flags()
	f.Uint64Var(&opts.Version, "version", 0, "deletes the specified version in migrations")
	f.BoolVar(&opts.All, "all", false, "clears all migrations for selected database")
	f.BoolVar(&opts.Force, "force", false, "when set executes operation without any confirmation")
	f.BoolVar(&opts.OnlyServer, "server", false, "to reset migrations only on server")

	return migrateDeleteCmd
}

type MigrateDeleteOptions struct {
	EC         *cli.ExecutionContext
	Version    uint64
	All        bool
	Force      bool
	OnlyServer bool

	Source cli.Source
}

func (o *MigrateDeleteOptions) Run() error {
	var op errors.Op = "commands.MigrateDeleteOptions.Run"
	o.EC.Spin("Removing migrations")
	defer o.EC.Spinner.Stop()
	if o.EC.AllDatabases {
		sourcesAndKind, err := metadatautil.GetSourcesAndKind(o.EC.APIClient.V1Metadata.ExportMetadata)
		if err != nil {
			return errors.E(op, fmt.Errorf("got error while getting the sources list : %v", err))
		}
		for _, source := range sourcesAndKind {
			o.Source = cli.Source(source)
			err := o.RunOnSource()
			if err != nil {
				return errors.E(op, fmt.Errorf("error while deleting status for database '%s': %v", o.Source.Name, err))
			}
		}
		return nil
	}
	o.Source = o.EC.Source
	if err := o.RunOnSource(); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (o *MigrateDeleteOptions) RunOnSource() error {
	var op errors.Op = "commands.MigrateDeleteOptions.RunOnSource"
	o.EC.Spin("Deleting migration...")

	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return errors.E(op, fmt.Errorf("error in creation of new migrate instance %w", err))
	}

	status, err := migrateDrv.GetStatus()
	if err != nil {
		return errors.E(op, fmt.Errorf("error while retrieving migration status %w", err))
	}

	// sourceVersions migration versions in source to be deleted similarly with serverVersions
	var sourceVersions, serverVersions []uint64

	if !o.All {
		// if o.version isn't present on source and on server return error version isn't present.
		if _, ok := status.Migrations[o.Version]; !ok {
			return errors.E(op, fmt.Errorf("version %v not found", o.Version))
		}
		sourceVersions = []uint64{o.Version}
		serverVersions = []uint64{o.Version}
	} else if o.All {
		for k, v := range status.Migrations {
			if v.IsApplied {
				serverVersions = append(serverVersions, k)
			}
			if v.IsPresent {
				sourceVersions = append(sourceVersions, k)
			}
		}
	}

	// resets the migrations on server
	err = migrateDrv.RemoveVersions(serverVersions)
	if err != nil {
		return errors.E(op, fmt.Errorf("error removing migration from server: %w", err))
	}

	// removes the migrations on source
	if !o.OnlyServer {
		err = DeleteVersions(o.EC, sourceVersions, o.Source)
		if err != nil {
			return errors.E(op, fmt.Errorf("error removing migrations from project: %w", err))
		}
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Infof("Deleted migrations")
	return nil
}

func DeleteVersions(ec *cli.ExecutionContext, versions []uint64, source cli.Source) error {
	var op errors.Op = "commands.DeleteVersions"
	for _, v := range versions {
		delOptions := mig.CreateOptions{
			Version:   strconv.FormatUint(v, 10),
			Directory: filepath.Join(ec.MigrationDir, source.Name),
		}
		err := delOptions.Delete()
		if err != nil {
			return errors.E(op, fmt.Errorf("unable to delete migrations from project for: %v : %w", v, err))
		}
	}
	return nil
}
