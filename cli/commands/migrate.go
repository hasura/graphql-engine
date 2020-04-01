package commands

import (
	"fmt"
	"net/url"
	"runtime"
	"strings"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata"
	"github.com/hasura/graphql-engine/cli/metadata/actions"
	"github.com/hasura/graphql-engine/cli/metadata/allowlist"
	"github.com/hasura/graphql-engine/cli/metadata/functions"
	"github.com/hasura/graphql-engine/cli/metadata/querycollections"
	"github.com/hasura/graphql-engine/cli/metadata/remoteschemas"
	"github.com/hasura/graphql-engine/cli/metadata/tables"
	metadataTypes "github.com/hasura/graphql-engine/cli/metadata/types"
	metadataVersion "github.com/hasura/graphql-engine/cli/metadata/version"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	// Initialize migration drivers
	_ "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	_ "github.com/hasura/graphql-engine/cli/migrate/source/file"
)

// NewMigrateCmd returns the migrate command
func NewMigrateCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	migrateCmd := &cobra.Command{
		Use:          "migrate",
		Short:        "Manage migrations on the database",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			cmd.Root().PersistentPreRun(cmd, args)
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
	}
	migrateCmd.AddCommand(
		newMigrateApplyCmd(ec),
		newMigrateStatusCmd(ec),
		newMigrateCreateCmd(ec),
		newMigrateSquashCmd(ec),
	)
	migrateCmd.PersistentFlags().String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	migrateCmd.PersistentFlags().String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	migrateCmd.PersistentFlags().String("access-key", "", "access key for Hasura GraphQL Engine")
	migrateCmd.PersistentFlags().MarkDeprecated("access-key", "use --admin-secret instead")

	v.BindPFlag("endpoint", migrateCmd.PersistentFlags().Lookup("endpoint"))
	v.BindPFlag("admin_secret", migrateCmd.PersistentFlags().Lookup("admin-secret"))
	v.BindPFlag("access_key", migrateCmd.PersistentFlags().Lookup("access-key"))
	return migrateCmd
}

func newMigrate(ec *cli.ExecutionContext, isCmd bool) (*migrate.Migrate, error) {
	dbURL := getDataPath(ec.Config.ServerConfig.ParsedEndpoint, getAdminSecretHeaderName(ec.Version), ec.Config.ServerConfig.AdminSecret)
	fileURL := getFilePath(ec.MigrationDir)
	t, err := migrate.New(fileURL.String(), dbURL.String(), isCmd, int(ec.Config.Version), ec.Logger)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}
	// Set Plugins
	setMetadataPluginsWithDir(ec, t)
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
	if v.ServerFeatureFlags.HasAccessKey {
		return XHasuraAccessKey
	}
	return XHasuraAdminSecret
}

func setMetadataPluginsWithDir(ec *cli.ExecutionContext, drv *migrate.Migrate, dir ...string) {
	var metadataDir string
	if len(dir) == 0 {
		metadataDir = ec.MetadataDir
	} else {
		metadataDir = dir[0]
	}
	plugins := make(metadataTypes.MetadataPlugins, 0)
	if ec.Config.Version == cli.V2 && metadataDir != "" {
		plugins = append(plugins, metadataVersion.New(ec, metadataDir))
		plugins = append(plugins, tables.New(ec, metadataDir))
		plugins = append(plugins, functions.New(ec, metadataDir))
		plugins = append(plugins, querycollections.New(ec, metadataDir))
		plugins = append(plugins, allowlist.New(ec, metadataDir))
		plugins = append(plugins, remoteschemas.New(ec, metadataDir))
		plugins = append(plugins, actions.New(ec, metadataDir))
	} else {
		plugins = append(plugins, metadata.New(ec, ec.MigrationDir))
	}
	drv.SetMetadataPlugins(plugins)
}
