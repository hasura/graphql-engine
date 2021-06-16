package commands

import (
	"bytes"
	"fmt"
	"path/filepath"
	"strings"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/hasura/graphql-engine/cli/v2/migrate"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/pkg/errors"
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
		Long:  "(PREVIEW) Squash multiple migrations leading up to the latest one into a single migration file",
		Example: `  # NOTE: This command is in PREVIEW. Correctness is not guaranteed and the usage may change.

  # squash all migrations from version 123 to the latest one:
  hasura migrate squash --from 123

  # Add a name for the new squashed migration
  hasura migrate squash --name "<name>" --from 123`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			return validateConfigV3Flags(cmd, ec)
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.newVersion = getTime()
			opts.Source = ec.Source
			if opts.EC.HasMetadataV3 && opts.EC.Config.Version < cli.V2 {
				return fmt.Errorf("squashing when using metadata V3 is supported from Config V2 only")
			}
			return opts.run()
		},
	}

	f := migrateSquashCmd.Flags()
	f.Uint64Var(&opts.from, "from", 0, "start squashing from this version")
	f.StringVar(&opts.name, "name", "squashed", "name for the new squashed migration")
	f.BoolVar(&opts.deleteSource, "delete-source", false, "delete the source files after squashing without any confirmation")

	// mark flag as required
	migrateSquashCmd.MarkFlagRequired("from")

	return migrateSquashCmd
}

type migrateSquashOptions struct {
	EC *cli.ExecutionContext

	from       uint64
	name       string
	newVersion int64

	deleteSource bool
	Source       cli.Source
}

func (o *migrateSquashOptions) run() error {
	o.EC.Logger.Warnln("This command is currently experimental and hence in preview, correctness of squashed migration is not guaranteed!")
	o.EC.Spin(fmt.Sprintf("Squashing migrations from %d to latest...", o.from))
	defer o.EC.Spinner.Stop()
	migrateDrv, err := migrate.NewMigrate(o.EC, true, o.Source.Name, o.Source.Kind)
	if err != nil {
		return errors.Wrap(err, "unable to initialize migrations driver")
	}

	versions, err := mig.SquashCmd(migrateDrv, o.from, o.newVersion, o.name, filepath.Join(o.EC.MigrationDir, o.Source.Name))
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.Wrap(err, "unable to squash migrations")
	}

	// squashed migration is generated
	// TODO: capture keyboard interrupt and offer to delete the squashed migration

	o.EC.Logger.Infof("Created '%d_%s' after squashing '%d' till '%d'", o.newVersion, o.name, versions[0], versions[len(versions)-1])

	if !o.deleteSource {
		ok := ask2confirmDeleteMigrations(versions, o.EC.Logger)
		if !ok {
			return nil
		}
	}

	var uversions []uint64
	for _, version := range versions {
		if version < 0 {
			return fmt.Errorf("operation failed foound version value should >= 0, which is not expected")
		}
		uversions = append(uversions, uint64(version))
	}

	// If the first argument is true then it deletes all the migration versions
	err = DeleteVersions(o.EC, uversions, o.Source)
	if err != nil {
		return err
	}

	err = migrateDrv.RemoveVersions(uversions)
	if err != nil {
		return err
	}
	return nil
}

func ask2confirmDeleteMigrations(versions []int64, log *logrus.Logger) bool {
	var s string

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
	log.Infof("Do you want to delete these migration source files? (y/N)")

	_, err := fmt.Scan(&s)
	if err != nil {
		log.Error("unable to take user input, skipping deleting files")
		return false
	}

	s = strings.TrimSpace(s)
	s = strings.ToLower(s)

	if s == "y" || s == "yes" {
		return true
	}
	return false
}
