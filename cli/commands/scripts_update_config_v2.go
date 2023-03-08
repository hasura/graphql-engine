package commands

import (
	"bytes"
	"fmt"
	"path/filepath"

	"io/ioutil"
	"os"

	"github.com/hasura/graphql-engine/cli/v2/internal/cliext"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"
	"github.com/hasura/graphql-engine/cli/v2/migrate"

	"gopkg.in/yaml.v3"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/v2/migrate/database/hasuradb"
	"github.com/hasura/graphql-engine/cli/v2/migrate/source"
	"github.com/hasura/graphql-engine/cli/v2/migrate/source/file"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newScriptsUpdateConfigV2Cmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	var metadataDir string
	scriptsUpdateConfigV2Cmd := &cobra.Command{
		Use:     "update-project-v2",
		Aliases: []string{"update-config-v2"},
		Short:   "Update the Hasura Project from config v1 to v2",
		Long: `Update the Hasura Project from config v1 to v2 by executing the following actions:

1. Installs a plugin system for CLI
2. Installs CLI Extensions plugins (primarily for actions)
3. Takes a back up of migrations directory
4. Removes all metadata yaml migrations and converts everything to SQL
5. Exports the metadata from server in the new format (multiple files in a directory)
6. Re-write the config.yaml file to new format
`,
		Example: `  # Read more about v2 configuration for CLI at https://docs.hasura.io

  # Update the Hasura Project from config v1 to v2
  hasura scripts update-project-v2

  # Update the Hasura Project from config v1 to v2 with a different metadata directory:
  hasura scripts update-project-v2 --metadata-dir "metadata"`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return errors.E(op, err)
			}
			if err := ec.Validate(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if ec.Config.Version != cli.V1 {
				return errors.E(op, fmt.Errorf("this script can be executed only when the current config version is 1"))
			}
			ec.Spin("Setting up cli-ext")
			defer ec.Spinner.Stop()
			err := cliext.Setup(ec)
			if err != nil {
				return errors.E(op, err)
			}
			// Move copy migrations directory to migrations_backup
			ec.Spin("Backing up migrations...")
			err = util.CopyDir(ec.MigrationDir, filepath.Join(ec.ExecutionDirectory, "migrations_backup"))
			if err != nil {
				return errors.E(op, fmt.Errorf("error in copying migrations to migrations_backup: %w", err))
			}
			defer func() {
				if err != nil {
					ec.Logger.Infof("migrations are backed up to migrations_backup directory.")
				}
			}()
			// Open the file driver to list of source migrations and remove unwanted yaml
			ec.Spin("Cleaning up migrations...")
			fileCfg, err := file.New(migrate.GetFilePath(ec.MigrationDir).String(), ec.Logger)
			if err != nil {
				return errors.E(op, fmt.Errorf("error in opening migrate file driver: %w", err))
			}
			err = fileCfg.Scan()
			if err != nil {
				return errors.E(op, fmt.Errorf("error in scanning migrate file driver: %w", err))
			}
			// Remove yaml from up migrations
			upVersions := make([]uint64, 0)
			for _, version := range fileCfg.Migrations.Index {
				sqlUp := &bytes.Buffer{}
				// check if up.yaml exists
				upMetaMigration, ok := fileCfg.Migrations.Migrations[version][source.MetaUp]
				if !ok {
					continue
				}
				// Read the up.yaml file
				bodyReader, _, _, err := fileCfg.ReadMetaUp(version)
				if err != nil {
					return errors.E(op, fmt.Errorf("error in reading %s file: %w", upMetaMigration.Raw, err))
				}
				buf := new(bytes.Buffer)
				_, err = buf.ReadFrom(bodyReader)
				if err != nil {
					return errors.E(op, fmt.Errorf("unable to read bytes: %w", err))
				}
				var queries []hasuradb.HasuraInterfaceQuery
				err = yaml.Unmarshal(buf.Bytes(), &queries)
				if err != nil {
					return errors.E(op, fmt.Errorf("unable to unmarshal %s: %w", upMetaMigration.Raw, err))
				}
				// for each query check if type is run_sql
				// if yes, append to bytes buffer
				for _, query := range queries {
					if query.Type == "run_sql" {
						argByt, err := yaml.Marshal(query.Args)
						if err != nil {
							return errors.E(op, fmt.Errorf("unable to marshal run_sql args in %s: %w", upMetaMigration.Raw, err))
						}
						var to hasura.PGRunSQLInput
						err = yaml.Unmarshal(argByt, &to)
						if err != nil {
							return errors.E(op, fmt.Errorf("unable to unmarshal run_sql args in %s: %w", upMetaMigration.Raw, err))
						}
						sqlUp.WriteString("\n")
						sqlUp.WriteString(to.SQL)
					}
				}
				// check if up.sql file exists
				if sqlUp.String() != "" {
					upMigration, ok := fileCfg.Migrations.Migrations[version][source.Up]
					if !ok {
						// if up.sql doesn't exists, create a up.sql file and upMigration
						var filePath string
						if upMetaMigration.IsDir {
							dir := filepath.Dir(upMetaMigration.Raw)
							filePath = filepath.Join(ec.MigrationDir, dir, "up.sql")
						} else {
							fileName := fmt.Sprintf("%d_%s.up.sql", version, upMetaMigration.Identifier)
							filePath = filepath.Join(ec.MigrationDir, fileName)
						}
						err = ioutil.WriteFile(filePath, sqlUp.Bytes(), os.ModePerm)
						if err != nil {
							return errors.E(op, fmt.Errorf("unable to create up migration: %w", err))
						}
						fileCfg.Migrations.Migrations[version][source.Up] = &source.Migration{}
					} else {
						filePath := filepath.Join(ec.MigrationDir, upMigration.Raw)
						upByt, err := ioutil.ReadFile(filePath)
						if err != nil {
							return errors.E(op, fmt.Errorf("error in reading up.sql: %w", err))
						}
						upByt = append(upByt, sqlUp.Bytes()...)
						err = ioutil.WriteFile(filePath, upByt, os.ModePerm)
						if err != nil {
							return errors.E(op, fmt.Errorf("error in writing up.sql: %w", err))
						}
					}
				}
				// delete the yaml file
				err = os.Remove(filepath.Join(ec.MigrationDir, upMetaMigration.Raw))
				if err != nil {
					return errors.E(op, fmt.Errorf("error in removing up.yaml: %w", err))
				}
				delete(fileCfg.Migrations.Migrations[version], source.MetaUp)
			}
			// Remove yaml from down migrations
			for _, version := range fileCfg.Migrations.Index {
				sqlDown := &bytes.Buffer{}
				downMetaMigration, ok := fileCfg.Migrations.Migrations[version][source.MetaDown]
				if !ok {
					continue
				}
				bodyReader, _, _, err := fileCfg.ReadMetaDown(version)
				if err != nil {
					return errors.E(op, fmt.Errorf("error in reading %s file: %w", downMetaMigration.Raw, err))
				}
				buf := new(bytes.Buffer)
				_, err = buf.ReadFrom(bodyReader)
				if err != nil {
					return errors.E(op, fmt.Errorf("unable to read bytes: %w", err))
				}
				var queries []hasuradb.HasuraInterfaceQuery
				err = yaml.Unmarshal(buf.Bytes(), &queries)
				if err != nil {
					return errors.E(op, fmt.Errorf("unable to unmarshal %s: %w", downMetaMigration.Raw, err))
				}
				for _, query := range queries {
					if query.Type == "run_sql" {
						argByt, err := yaml.Marshal(query.Args)
						if err != nil {
							return errors.E(op, fmt.Errorf("unable to marshal run_sql args in %s: %w", downMetaMigration.Raw, err))
						}
						var to hasura.PGRunSQLInput
						err = yaml.Unmarshal(argByt, &to)
						if err != nil {
							return errors.E(op, fmt.Errorf("unable to unmarshal run_sql args in %s: %w", downMetaMigration.Raw, err))
						}
						sqlDown.WriteString("\n")
						sqlDown.WriteString(to.SQL)
					}
				}
				// check if up.sql file exists
				if sqlDown.String() != "" {
					downMigration, ok := fileCfg.Migrations.Migrations[version][source.Down]
					if !ok {
						// if up.sql doesn't exists, create a up.sql file and upMigration
						var filePath string
						if downMetaMigration.IsDir {
							dir := filepath.Dir(downMetaMigration.Raw)
							filePath = filepath.Join(ec.MigrationDir, dir, "down.sql")
						} else {
							fileName := fmt.Sprintf("%d_%s.down.sql", version, downMetaMigration.Identifier)
							filePath = filepath.Join(ec.MigrationDir, fileName)
						}
						err = ioutil.WriteFile(filePath, sqlDown.Bytes(), os.ModePerm)
						if err != nil {
							return errors.E(op, fmt.Errorf("unable to create up migration: %w", err))
						}
						fileCfg.Migrations.Migrations[version][source.Down] = &source.Migration{}
					} else {
						filePath := filepath.Join(ec.MigrationDir, downMigration.Raw)
						downByt, err := ioutil.ReadFile(filePath)
						if err != nil {
							return errors.E(op, fmt.Errorf("error in reading down.sql: %w", err))
						}
						downByt = append(sqlDown.Bytes(), downByt...)
						err = ioutil.WriteFile(filePath, downByt, os.ModePerm)
						if err != nil {
							return errors.E(op, fmt.Errorf("error in writing down.sql: %w", err))
						}
					}
				}
				// delete the yaml file
				err = os.Remove(filepath.Join(ec.MigrationDir, downMetaMigration.Raw))
				if err != nil {
					return errors.E(op, fmt.Errorf("error in removing down.yaml: %w", err))
				}
				delete(fileCfg.Migrations.Migrations[version], source.MetaDown)
			}
			for version := range fileCfg.Migrations.Migrations {
				directions := fileCfg.GetDirections(version)
				// check if all the directions were set, else delete
				if !directions[source.Up] && !directions[source.MetaUp] && !directions[source.Down] && !directions[source.MetaDown] {
					files, err := filepath.Glob(filepath.Join(ec.MigrationDir, fmt.Sprintf("%d_*", version)))
					if err != nil {
						return errors.E(op, fmt.Errorf("unable to filter files for %d: %w", version, err))
					}
					for _, file := range files {
						info, err := os.Stat(file)
						if err != nil {
							return errors.E(op, fmt.Errorf("error in stating file: %w", err))
						}
						if info.IsDir() {
							err = os.RemoveAll(file)
							if err != nil {
								return errors.E(op, fmt.Errorf("error in removing dir: %w", err))
							}
						} else {
							if err := os.Remove(file); err != nil {
								return errors.E(op, fmt.Errorf("error in removing file: %w", err))
							}
						}
					}
					upVersions = append(upVersions, version)
				}
			}
			ec.Spin("Removing versions from database...")
			migrateDrv, err := migrate.NewMigrate(ec, true, "", hasura.SourceKindPG)
			if err != nil {
				return errors.E(op, fmt.Errorf("unable to initialize migrations driver: %w", err))
			}
			err = migrateDrv.RemoveVersions(upVersions)
			if err != nil {
				return errors.E(op, fmt.Errorf("unable to remove versions from database: %w", err))
			}
			// update current config to v2
			ec.Spin("Updating current config to 2")
			os.Setenv("HASURA_GRAPHQL_VERSION", "2")
			os.Setenv("HASURA_GRAPHQL_METADATA_DIRECTORY", metadataDir)
			os.Setenv("HASURA_GRAPHQL_ACTION_KIND", ec.Viper.GetString("actions.kind"))
			os.Setenv("HASURA_GRAPHQL_ACTION_HANDLER_WEBHOOK_BASEURL", ec.Viper.GetString("actions.handler_webhook_baseurl"))
			defer func() {
				// unset env
				os.Unsetenv("HASURA_GRAPHQL_VERSION")
				os.Unsetenv("HASURA_GRAPHQL_METADATA_DIRECTORY")
				os.Unsetenv("HASURA_GRAPHQL_ACTION_KIND")
				os.Unsetenv("HASURA_GRAPHQL_ACTION_HANDLER_WEBHOOK_BASEURL")
			}()
			ec.Spin("Reloading config file...")
			err = ec.Validate()
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot validate new config: %w", err))
			}
			defer func() {
				if err != nil {
					os.RemoveAll(ec.MetadataDir)
				}
			}()
			// set codegen to nil, so that it is not exported in yaml
			ec.Config.ActionConfig.Codegen = nil
			// run metadata export
			ec.Spin("Exporting metadata...")
			var files map[string][]byte
			mdHandler := projectmetadata.NewHandlerFromEC(ec)
			files, err = mdHandler.ExportMetadata()
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot export metadata from server: %w", err))
			}
			ec.Spin("Writing metadata...")
			err = mdHandler.WriteMetadata(files)
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot write metadata: %w", err))
			}
			ec.Spin("Writing new config file...")
			// Read the config from config.yaml
			cfgByt, err := ioutil.ReadFile(ec.ConfigFile)
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot read config file: %w", err))
			}
			var cfg cli.Config
			err = yaml.Unmarshal(cfgByt, &cfg)
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot parse config file: %w", err))
			}
			cfg.Version = cli.V2
			cfg.MetadataDirectory = ec.Viper.GetString("metadata_directory")
			cfg.ActionConfig = &types.ActionExecutionConfig{
				Kind:                  ec.Viper.GetString("actions.kind"),
				HandlerWebhookBaseURL: ec.Viper.GetString("actions.handler_webhook_baseurl"),
			}
			err = ec.WriteConfig(&cfg)
			if err != nil {
				return errors.E(op, fmt.Errorf("cannot write config file: %w", err))
			}
			ec.Spinner.Stop()
			ec.Logger.Infoln("Updated config to version 2")

			if f, _ := os.Stat(filepath.Join(ec.MigrationDir, "metadata.yaml")); f != nil {
				err = os.Remove(filepath.Join(ec.MigrationDir, "metadata.yaml"))
				if err != nil {
					ec.Logger.Warnln("Warning: cannot remove metadata.yaml file ", err)
				}
			}

			return nil
		},
	}

	f := scriptsUpdateConfigV2Cmd.Flags()

	f.StringVar(&metadataDir, "metadata-dir", "metadata", "")
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
	f.Bool("insecure-skip-tls-verify", false, "skip TLS verification and disable cert checking (default: false)")
	f.String("certificate-authority", "", "path to a cert file for the certificate authority")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))

	return scriptsUpdateConfigV2Cmd
}
