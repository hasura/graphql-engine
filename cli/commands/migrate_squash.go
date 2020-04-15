package commands

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli/migrate"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
)

func newMigrateSquashCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &migrateSquashOptions{
		EC: ec,
	}
	migrateSquashCmd := &cobra.Command{
		Use:   "squash",
		Short: "(PREVIEW) Squash multiple migrations into a single one",
		Long:  "(PREVIEW) Squash multiple migrations leading upto the latest one into a single migration file",
		Example: `  # NOTE: This command is in PREVIEW, correctness is not guaranteed and the usage may change.

  # squash all migrations from version 123 to the latest one:
  hasura migrate squash --from 123

  # Add a name for the new squashed migration
  hasura migrate squash --name "<name>" --from 123`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.newVersion = getTime()
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
}

func (o *migrateSquashOptions) run() error {
	o.EC.Logger.Warnln("This command is currently experimental and hence in preview, correctness of squashed migration is not guaranteed!")
	o.EC.Spin(fmt.Sprintf("Squashing migrations from %d to latest...", o.from))
	defer o.EC.Spinner.Stop()
	migrateDrv, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return errors.Wrap(err, "unable to initialize migrations driver")
	}

	versions, err := mig.SquashCmd(migrateDrv, o.from, o.newVersion, o.name, o.EC.MigrationDir)
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

	for _, v := range versions {
		delOptions := mig.CreateOptions{
			Version:   strconv.FormatInt(v, 10),
			Directory: o.EC.MigrationDir,
		}
		err = delOptions.Delete()
		if err != nil {
			return errors.Wrap(err, "unable to delete source file")
		}
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
