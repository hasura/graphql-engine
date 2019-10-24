package commands

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	log "github.com/sirupsen/logrus"
)

func newMigrateSquashCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateSquashOptions{
		EC: ec,
	}
	migrateSquashCmd := &cobra.Command{
		Use:   "squash",
		Short: "Squash multiple migrations into a single one",
		Long:  "Squash multiple migrations leading upto the latest one into a single migration file",
		Example: `  # squash all migrations from version 123 to the latest one:
  hasura migrate squash --from 123`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.newVersion = getTime()
			return opts.run()
		},
	}

	f := migrateSquashCmd.Flags()
	f.Uint64Var(&opts.from, "from", 0, "start squashing form this version")
	f.StringVar(&opts.name, "name", "squashed", "name for the new squashed migration")
	f.BoolVar(&opts.deleteSource, "delete-source", "delete the source files after squashing without any confirmation")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

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
	o.EC.Spin(fmt.Sprintf("Squashing migrations from %d to latest...", o.from))
	defer o.EC.Spinner.Stop()
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return errors.Wrap(err, "unable to initialize migrations driver")
	}

	versions, err := mig.SquashCmd(migrateDrv, o.from, o.newVersion, o.name, o.EC.MigrationDir)
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.Wrap("unable to squash migrations")
	}

	o.EC.Logger.WithFields(log.Fields{
		"version": o.newVersion,
		"name":    o.name,
	}).Infof("Created a new migration after squashing %d till %d", versions[0], versions[len(versions)-1])

	if !opts.deleteSource {
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

	out := new(tabwriter.Writer)
	buf := &bytes.Buffer{}
	out.Init(buf, 0, 8, 2, ' ', 0)
	w := util.NewPrefixWriter(out)
	w.Write(util.LEVEL_0, "VERSION\n")
	for _, version := range versions {
		w.Write(util.LEVEL_0, "%d\n",
			version,
		)
	}
	out.Flush()
	fmt.Println(buf.String())
	log.Infof("These migrations are squashed into a new one. Do you want to delete the source files? (y/N)")

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
