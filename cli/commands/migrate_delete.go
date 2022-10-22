package commands

import (
	"fmt"
	"path/filepath"
	"strconv"

	"github.com/hasura/graphql-engine/cli/v2"
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
		Example: `
  # Usage to delete a version:
  hasura migrate delete --version <version_delete> --database-name <database-name>
  
  # Usage to delete all versions
   hasura migrate delete --all --database-name <database-name>`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Logger.Warn("[PREVIEW] this command is in preview. usage may change in future\n")
			if err := validateConfigV3Flags(cmd, ec); err != nil {
				return err
			}
			if !cmd.Flags().Changed("all") && !cmd.Flags().Changed("version") {
				return fmt.Errorf("at least one flag [--all , --version] should be set")
			}
			if cmd.Flags().Changed("all") && cmd.Flags().Changed("version") {
				return fmt.Errorf("only one of [--all , --version] should be set")
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {

			// exit if user inputs n for clearing migrations
			if cmd.Flags().Changed("all") && !opts.force {
				confirmation, err := util.GetYesNoPrompt("clear all migrations of database and it's history on the server?")
				if err != nil {
					return fmt.Errorf("error getting user input: %w", err)
				}
				if confirmation == "n" {
					return nil
				}
			}

			opts.Source = ec.Source
			if ec.Config.Version >= cli.V3 {
				var err error
				opts.EC.Spin("Removing migrations")
				err = opts.Run()
				opts.EC.Spinner.Stop()
				if err != nil {
					return fmt.Errorf("operation failed: %w", err)
				}
				return err
			}
			opts.EC.Spin("Removing migrations")
			err := opts.Run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return fmt.Errorf("operation failed: %w", err)
			}
			return nil
		},
	}

	f := migrateDeleteCmd.Flags()
	f.Uint64Var(&opts.version, "version", 0, "deletes the specified version in migrations")
	f.BoolVar(&opts.all, "all", false, "clears all migrations for selected database")
	f.BoolVar(&opts.force, "force", false, "when set executes operation without any confirmation")
	f.BoolVar(&opts.onlyServer, "server", false, "to reset migrations only on server")

	return migrateDeleteCmd
}

type MigrateDeleteOptions struct {
	EC         *cli.ExecutionContext
	version    uint64
	all        bool
	force      bool
	onlyServer bool

	Source cli.Source
}

func (o *MigrateDeleteOptions) Run() error {
	o.EC.Spin("Deleting migration...")
	defer o.EC.Spinner.Stop()

	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return fmt.Errorf("error in creation of new migrate instance %w", err)
	}

	status, err := migrateDrv.GetStatus()
	if err != nil {
		return fmt.Errorf("error while retrieving migration status %w", err)
	}

	// sourceVersions migration versions in source to be deleted similarly with serverVersions
	var sourceVersions, serverVersions []uint64

	if !o.all {
		// if o.version isn't present on source and on server return error version isn't present.
		if _, ok := status.Migrations[o.version]; !ok {
			return fmt.Errorf("version %v not found", o.version)
		}
		sourceVersions = []uint64{o.version}
		serverVersions = []uint64{o.version}
	} else if o.all {
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
		return fmt.Errorf("error removing migration from server: %w", err)
	}

	// removes the migrations on source
	if !o.onlyServer {
		err = DeleteVersions(o.EC, sourceVersions, o.Source)
		if err != nil {
			return fmt.Errorf("error removing migrations from project: %w", err)
		}
	}

	o.EC.Logger.Infof("Deleted migrations")
	return nil
}

func DeleteVersions(ec *cli.ExecutionContext, versions []uint64, source cli.Source) error {
	for _, v := range versions {
		delOptions := mig.CreateOptions{
			Version:   strconv.FormatUint(v, 10),
			Directory: filepath.Join(ec.MigrationDir, source.Name),
		}
		err := delOptions.Delete()
		if err != nil {
			return fmt.Errorf("unable to delete migrations from project for: %v : %w", v, err)
		}
	}
	return nil
}
