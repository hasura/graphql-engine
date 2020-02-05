package commands

import (
	"fmt"
	"net/url"
	"runtime"
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	// Initialize migration drivers
	_ "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	_ "github.com/hasura/graphql-engine/cli/migrate/source/file"
)

// NewMigrateCmd returns the migrate command
func NewMigrateCmd(ec *cli.ExecutionContext) *cobra.Command {
	migrateCmd := &cobra.Command{
		Use:          "migrate",
		Short:        "Manage migrations on the database",
		SilenceUsage: true,
	}
	migrateCmd.AddCommand(
		newMigrateApplyCmd(ec),
		newMigrateStatusCmd(ec),
		newMigrateCreateCmd(ec),
		newMigrateSquashCmd(ec),
	)
	return migrateCmd
}

func newMigrate(dir string, db *url.URL, adminSecretValue string, logger *logrus.Logger, v *version.Version, isCmd bool) (*migrate.Migrate, error) {
	dbURL := getDataPath(db, getAdminSecretHeaderName(v), adminSecretValue)
	fileURL := getFilePath(dir)
	t, err := migrate.New(fileURL.String(), dbURL.String(), isCmd, logger)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}
	return t, nil
}

// ExecuteMigration runs the actual migration
func ExecuteMigration(cmd string, t *migrate.Migrate, stepOrVersion int64) error {
	var err error

	switch cmd {
	case "up":
		err = mig.UpCmd(t, stepOrVersion)
	case "down":
		err = mig.DownCmd(t, stepOrVersion)
	case "gotoVersion":
		err = mig.GotoVersionCmd(t, stepOrVersion)
	case "version":
		var direction string
		if stepOrVersion >= 0 {
			direction = "up"
		} else {
			direction = "down"
			stepOrVersion = -(stepOrVersion)
		}
		err = mig.GotoCmd(t, uint64(stepOrVersion), direction)
	default:
		err = fmt.Errorf("Invalid command")
	}

	return err
}

func executeStatus(t *migrate.Migrate) (*migrate.Status, error) {
	status, err := t.GetStatus()
	if err != nil {
		return nil, err
	}
	return status, nil
}

func getDataPath(nurl *url.URL, adminSecretHeader, adminSecretValue string) *url.URL {
	host := &url.URL{
		Scheme: "hasuradb",
		Host:   nurl.Host,
		Path:   nurl.Path,
	}
	q := nurl.Query()
	// Set sslmode in query
	switch scheme := nurl.Scheme; scheme {
	case "https":
		q.Set("sslmode", "enable")
	default:
		q.Set("sslmode", "disable")
	}
	if adminSecretValue != "" {
		q.Add("headers", fmt.Sprintf("%s:%s", adminSecretHeader, adminSecretValue))
	}
	host.RawQuery = q.Encode()
	return host
}

func getFilePath(dir string) *url.URL {
	host := &url.URL{
		Scheme: "file",
		Path:   dir,
	}

	// Add Prefix / to path if runtime.GOOS equals to windows
	if runtime.GOOS == "windows" && !strings.HasPrefix(host.Path, "/") {
		host.Path = "/" + host.Path
	}
	return host
}

const (
	XHasuraAdminSecret = "X-Hasura-Admin-Secret"
	XHasuraAccessKey   = "X-Hasura-Access-Key"
)

func getAdminSecretHeaderName(v *version.Version) string {
	if v.ServerSemver == nil {
		return XHasuraAdminSecret
	}
	flags, err := v.GetServerFeatureFlags()
	if err != nil {
		return XHasuraAdminSecret
	}
	if flags.HasAccessKey {
		return XHasuraAccessKey
	}
	return XHasuraAdminSecret
}
