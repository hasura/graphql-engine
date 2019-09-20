package commands

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"
	"text/tabwriter"

	"github.com/hasura/graphql-engine/cli"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMigrateSquashCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &migrateSquashOptions{
		EC: ec,
	}
	migrateSquashCmd := &cobra.Command{
		Use:          "squash",
		Short:        "",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.version = getTime()
			err := opts.run()
			if err != nil {
				return err
			}
			return nil
		},
	}

	f := migrateSquashCmd.Flags()
	f.Uint64Var(&opts.from, "from", 0, "squash from this version number")
	f.StringVar(&opts.name, "name", "default_squash", "name of the migration")
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

	from    uint64
	name    string
	version int64
}

func (o *migrateSquashOptions) run() error {
	o.EC.Spin("Squashing migrations...")
	defer o.EC.Spinner.Stop()
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}

	versions, err := mig.SquashCmd(migrateDrv, o.from, o.version, o.name, o.EC.MigrationDir)
	o.EC.Spinner.Stop()
	if err != nil {
		return err
	}

	o.EC.Logger.WithFields(log.Fields{
		"version": o.version,
		"name":    o.name,
	}).Info("Migrations files created")

	ok := ask2confirmDeleteMigrations(versions, o.EC.Logger)
	if !ok {
		return nil
	}

	for _, v := range versions {
		delOptions := mig.CreateOptions{
			Version:   strconv.FormatInt(v, 10),
			Directory: o.EC.MigrationDir,
		}
		err = delOptions.Delete()
		if err != nil {
			return err
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
	log.Infof("Do you want to delete the above list of squashed migrations? (y/N)")
	_, err := fmt.Scan(&s)
	if err != nil {
		log.Error("unable to take input, skipping deleting files")
		return false
	}

	s = strings.TrimSpace(s)
	s = strings.ToLower(s)

	if s == "y" || s == "yes" {
		return true
	}
	return false
}
