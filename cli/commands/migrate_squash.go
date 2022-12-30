package commands

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/hasura/graphql-engine/cli/v2/migrate"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	mig "github.com/hasura/graphql-engine/cli/v2/migrate/cmd"
)

func newMigrateSquashCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &migrateSquashOptions{
		EC: ec,
	}
	migrateSquashCmd := &cobra.Command{
		Use:   "squash",
		Short: "(PREVIEW) Squash multiple migrations into a single one",
		Long:  "As you're developing your Hasura GraphQL API, you may find yourself in a situation where you have a lot of migrations that you want to squash into a single one. This command helps you do that. By running this command, you can squash all the iterative migrations you've created into a single file.",
		Example: `  # NOTE: This command is in PREVIEW. Correctness is not guaranteed and the usage may change.

  # squash all migrations from version 123 to the latest one:
  hasura migrate squash --from 123

  # Add a name for the new squashed migration
  hasura migrate squash --name "<name>" --from 123`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if err := validateConfigV3Flags(cmd, ec); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.newVersion = getTime()
			opts.Source = ec.Source
			if opts.EC.HasMetadataV3 && opts.EC.Config.Version < cli.V2 {
				return errors.E(op, fmt.Errorf("squashing when using metadata V3 is supported from Config V2 only"))
			}
			if err := opts.run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	f := migrateSquashCmd.Flags()
	f.Uint64Var(&opts.from, "from", 0, "start squashing from this version")
	f.Int64Var(&opts.to, "to", -1, "squash up to this version")
	f.StringVar(&opts.name, "name", "squashed", "name for the new squashed migration")
	f.BoolVar(&opts.deleteSource, "delete-source", false, "delete the source files after squashing without any confirmation")

	// mark flag as required
	err := migrateSquashCmd.MarkFlagRequired("from")
	if err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}

	return migrateSquashCmd
}

type migrateSquashOptions struct {
	EC *cli.ExecutionContext

	from       uint64
	to         int64
	name       string
	newVersion int64

	deleteSource bool
	Source       cli.Source
}

func (o *migrateSquashOptions) run() error {
	var op errors.Op = "commands.migrateSquashOptions.run"
	o.EC.Logger.Warnln("This command is currently experimental and hence in preview, correctness of squashed migration is not guaranteed!")
	o.EC.Spin(fmt.Sprintf("Squashing migrations from %d to latest...", o.from))
	defer o.EC.Spinner.Stop()
	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return errors.E(op, fmt.Errorf("unable to initialize migrations driver: %w", err))
	}
	status, err := migrateDrv.GetStatus()
	if err != nil {
		return errors.E(op, fmt.Errorf("finding status: %w", err))
	}
	var toMigration, fromMigration *migrate.MigrationStatus
	fromMigration, ok := status.Read(o.from)
	if !ok {
		return errors.E(op, fmt.Errorf("validating 'from' migration failed. Make sure migration with version %v exists", o.from))
	}
	if o.to == -1 {
		toMigration = status.Migrations[status.Index[status.Index.Len()-1]]
	} else {
		var ok bool
		if int64(o.from) > o.to {
			return errors.E(op, fmt.Errorf("cannot squash from %v to %v: %v (from) should be less than %v (to)", o.from, o.to, o.from, o.to))
		}
		toMigration, ok = status.Read(uint64(o.to))
		if !ok {
			return errors.E(op, fmt.Errorf("validating 'to' migration failed. Make sure migration with version %v exists", o.to))
		}
	}
	if err := validateMigrations(status, fromMigration.Version, toMigration.Version); err != nil {
		return errors.E(op, err)
	}

	versions, err := mig.SquashCmd(migrateDrv, o.from, o.to, o.newVersion, o.name, filepath.Join(o.EC.MigrationDir, o.Source.Name))
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.E(op, fmt.Errorf("unable to squash migrations: %w", err))
	}

	var uversions []uint64
	for _, version := range versions {
		if version < 0 {
			return errors.E(op, fmt.Errorf("operation failed found version value should >= 0, which is not expected"))
		}
		uversions = append(uversions, uint64(version))
	}

	newSquashedMigrationsDestination := filepath.Join(o.EC.MigrationDir, o.Source.Name, fmt.Sprintf("squashed_%d_to_%d", uversions[0], uversions[len(uversions)-1]))
	err = os.MkdirAll(newSquashedMigrationsDestination, os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("creating directory to move squashed migrations: %w", err))
	}

	err = moveMigrations(o.EC, uversions, o.EC.Source, newSquashedMigrationsDestination)
	if err != nil {
		return errors.E(op, fmt.Errorf("moving squashed migrations: %w", err))
	}
	oldPath := filepath.Join(o.EC.MigrationDir, o.Source.Name, fmt.Sprintf("%d_%s", o.newVersion, o.name))
	newPath := filepath.Join(o.EC.MigrationDir, o.Source.Name, fmt.Sprintf("%d_%s", toMigration.Version, o.name))
	err = os.Rename(oldPath, newPath)
	if err != nil {
		return errors.E(op, fmt.Errorf("renaming squashed migrations: %w", err))
	}

	o.EC.Logger.Infof("Created '%d_%s' after squashing '%d' till '%d'", toMigration.Version, o.name, versions[0], versions[len(versions)-1])

	if !o.deleteSource && o.EC.IsTerminal {
		o.deleteSource = ask2confirmDeleteMigrations(versions, newSquashedMigrationsDestination, o.EC.Logger)
	}
	if o.deleteSource {
		// If the first argument is true then it deletes all the migration versions
		err = os.RemoveAll(newSquashedMigrationsDestination)
		if err != nil {
			o.EC.Logger.Errorf("deleting directory %v failed: %v", newSquashedMigrationsDestination, err)
		}

		// remove everything but the last one from squashed migrations list from state store
		err = migrateDrv.RemoveVersions(uversions[:len(uversions)-1])
		if err != nil {
			o.EC.Logger.Errorf("removing squashed migration state from server failed: %v", err)
		}
	}

	return nil
}

func validateMigrations(status *migrate.Status, from uint64, to uint64) error {
	var op errors.Op = "commands.validateMigrations"
	// do not allow squashing a set of migrations when they are out of sync
	// ie if I want to squash the following set of migrations
	// 1
	// 2
	// 3
	// either all of them should be applied on the database / none of them should be applied
	// ie we will not allow states like
	// 1 applied
	// 2 not applied
	// 3 applied
	var fromIndex, toIndex int
	for idx, m := range status.Index {
		if m == from {
			fromIndex = idx
		}
		if m == to {
			toIndex = idx
		}
	}
	prevApplied := status.Migrations[status.Index[fromIndex]].IsApplied
	for idx := fromIndex + 1; idx <= toIndex; idx++ {
		migration := status.Migrations[status.Index[idx]]
		if !(migration.IsApplied == prevApplied && migration.IsPresent) {
			return errors.E(op, fmt.Errorf("migrations are out of sync. all migrations selected to squash should be applied or all should be be unapplied. found first mismatch at %v. use 'hasura migrate status' to inspect", migration.Version))
		}
	}

	return nil
}

func moveMigrations(ec *cli.ExecutionContext, versions []uint64, source cli.Source, destination string) error {
	var op errors.Op = "commands.moveMigrations"
	for _, v := range versions {
		moveOpts := mig.CreateOptions{
			Version:   strconv.FormatUint(v, 10),
			Directory: filepath.Join(ec.MigrationDir, source.Name),
		}
		err := moveOpts.MoveToDir(destination)
		if err != nil {
			return errors.E(op, fmt.Errorf("unable to move migrations from project for: %v : %w", v, err))
		}
	}
	return nil
}

func ask2confirmDeleteMigrations(versions []int64, squashedDirectoryName string, log *logrus.Logger) bool {
	log.Infof("The following migrations are squashed into a new one:")

	out := new(tabwriter.Writer)
	buf := &bytes.Buffer{}
	out.Init(buf, 0, 8, 2, ' ', 0)
	w := util.NewPrefixWriter(out)
	for _, version := range versions {
		w.Write(util.LEVEL_0, "%d\n",
			version,
		)
	}
	_ = out.Flush()
	fmt.Println(buf.String())
	question := fmt.Sprintf("migrations which were squashed is moved to %v. Delete them permanently?", filepath.Base(squashedDirectoryName))
	resp, err := util.GetYesNoPrompt(question)
	if err != nil {
		log.Errorf("error getting user input: %v", err)
		return false
	}
	return resp
}
