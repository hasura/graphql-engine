package commands

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v2"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	"github.com/hasura/graphql-engine/cli/migrate/source"
	"github.com/hasura/graphql-engine/cli/migrate/source/file"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newScriptsUpdateConfigV2Cmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	var metadataDir string
	scriptsUpdateConfigV2Cmd := &cobra.Command{
		Use:     "update-project-v2",
		Aliases: []string{"update-config-v2"},
		Short:   "Update the Hasura Project from v1 to v2",
		Long: `Update the Hasura Project from v1 to v2 by executing the following actions:
1. Installs a plugin system for CLI
2. Installs CLI Extensions plugins (primarily for actions)
3. Takes a back up of migrations directory
4. Removes all metadata yaml migrations and converts everything to SQL
5. Exports the metadata from server in the new format (multiple files in a directory)
6. Re-write the config.yaml file to new format
`,
		Example: `  # Read more about v2 configuration for CLI at https://docs.hasura.io

  # Update the Hasura Project from v1 to v2
  hasura scripts update-project-v2

  # Update the Hasura Project from v1 to v2 with a different metadata directory:
  hasura scripts update-project-v2 --metadata-dir "metadata"`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			if ec.Config.Version != cli.V1 {
				return fmt.Errorf("this script can be executed only when the current config version is 1")
			}
			// update the plugin index
			ec.Spin("Updating the plugin index...")
			defer ec.Spinner.Stop()
			err := ec.PluginsConfig.Repo.EnsureUpdated()
			if err != nil {
				return errors.Wrap(err, "cannot update plugin index")
			}
			// install the plugin
			ec.Spin("Installing cli-ext plugin...")
			err = ec.InstallPlugin("cli-ext", true)
			if err != nil {
				return errors.Wrap(err, "cannot install plugin")
			}
			// Move copy migrations directory to migrations_backup
			ec.Spin("Backing up migrations...")
			err = util.CopyDir(ec.MigrationDir, filepath.Join(ec.ExecutionDirectory, "migrations_backup"))
			if err != nil {
				return errors.Wrap(err, "error in copying migrations to migrations_backup")
			}
			defer func() {
				if err != nil {
					ec.Logger.Infof("migrations are backed up to migrations_backup directory.")
				}
			}()
			// Open the file driver to list of source migrations and remove unwanted yaml
			ec.Spin("Cleaning up migrations...")
			fileCfg, err := file.New(getFilePath(ec.MigrationDir).String(), ec.Logger)
			if err != nil {
				return errors.Wrap(err, "error in opening migrate file driver")
			}
			err = fileCfg.Scan()
			if err != nil {
				return errors.Wrap(err, "error in scanning migrate file driver")
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
					return errors.Wrapf(err, "error in reading %s file", upMetaMigration.Raw)
				}
				buf := new(bytes.Buffer)
				_, err = buf.ReadFrom(bodyReader)
				if err != nil {
					return errors.Wrapf(err, "unable to read bytes")
				}
				var queries []hasuradb.HasuraInterfaceQuery
				err = yaml.Unmarshal(buf.Bytes(), &queries)
				if err != nil {
					return errors.Wrapf(err, "unable to unmarhsal %s", upMetaMigration.Raw)
				}
				// for each query check if type is run_sql
				// if yes, append to bytes buffer
				for _, query := range queries {
					if query.Type == "run_sql" {
						argByt, err := yaml.Marshal(query.Args)
						if err != nil {
							return errors.Wrapf(err, "unable to marshal run_sql args in %s", upMetaMigration.Raw)
						}
						var to hasuradb.RunSQLInput
						err = yaml.Unmarshal(argByt, &to)
						if err != nil {
							return errors.Wrapf(err, "unable to unmarshal run_sql args in %s", upMetaMigration.Raw)
						}
						sqlUp.WriteString("\n")
						sqlUp.WriteString(to.SQL)
					}
				}
				// check if up.sql file exists
				if string(sqlUp.Bytes()) != "" {
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
							return errors.Wrap(err, "unable to create up migration")
						}
						fileCfg.Migrations.Migrations[version][source.Up] = &source.Migration{}
					} else {
						upByt, err := ioutil.ReadFile(upMigration.Raw)
						if err != nil {
							return errors.Wrap(err, "error in reading up.sql")
						}
						upByt = append(upByt, sqlUp.Bytes()...)
						err = ioutil.WriteFile(upMigration.Raw, upByt, os.ModePerm)
						if err != nil {
							return errors.Wrap(err, "error in writing up.sql")
						}
					}
				}
				// delete the yaml file
				err = os.Remove(filepath.Join(ec.MigrationDir, upMetaMigration.Raw))
				if err != nil {
					return errors.Wrap(err, "error in removing up.yaml")
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
					return errors.Wrapf(err, "error in reading %s file", downMetaMigration.Raw)
				}
				buf := new(bytes.Buffer)
				_, err = buf.ReadFrom(bodyReader)
				if err != nil {
					return errors.Wrap(err, "unable to read bytes")
				}
				var queries []hasuradb.HasuraInterfaceQuery
				err = yaml.Unmarshal(buf.Bytes(), &queries)
				if err != nil {
					return errors.Wrapf(err, "unable to unmarhsal %s", downMetaMigration.Raw)
				}
				for _, query := range queries {
					if query.Type == "run_sql" {
						argByt, err := yaml.Marshal(query.Args)
						if err != nil {
							return errors.Wrapf(err, "unable to marshal run_sql args in %s", downMetaMigration.Raw)
						}
						var to hasuradb.RunSQLInput
						err = yaml.Unmarshal(argByt, &to)
						if err != nil {
							return errors.Wrapf(err, "unable to unmarshal run_sql args in %s", downMetaMigration.Raw)
						}
						sqlDown.WriteString("\n")
						sqlDown.WriteString(to.SQL)
					}
				}
				// check if up.sql file exists
				if string(sqlDown.Bytes()) != "" {
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
							return errors.Wrap(err, "unable to create up migration")
						}
						fileCfg.Migrations.Migrations[version][source.Down] = &source.Migration{}
					} else {
						downByt, err := ioutil.ReadFile(downMigration.Raw)
						if err != nil {
							return errors.Wrap(err, "error in reading down.sql")
						}
						downByt = append(sqlDown.Bytes(), downByt...)
						err = ioutil.WriteFile(downMigration.Raw, downByt, os.ModePerm)
						if err != nil {
							return errors.Wrap(err, "error in writing down.sql")
						}
					}
				}
				// delete the yaml file
				err = os.Remove(filepath.Join(ec.MigrationDir, downMetaMigration.Raw))
				if err != nil {
					return errors.Wrap(err, "error in removing down.yaml")
				}
				delete(fileCfg.Migrations.Migrations[version], source.MetaDown)
			}
			for version := range fileCfg.Migrations.Migrations {
				directions := fileCfg.GetDirections(version)
				// check if all the directions were set, else delete
				if !directions[source.Up] && !directions[source.MetaUp] && !directions[source.Down] && !directions[source.MetaDown] {
					files, err := filepath.Glob(filepath.Join(ec.MigrationDir, fmt.Sprintf("%d_*", version)))
					if err != nil {
						return errors.Wrapf(err, "unable to filter files for %d", version)
					}
					for _, file := range files {
						info, err := os.Stat(file)
						if err != nil {
							return errors.Wrap(err, "error in stating file")
						}
						if info.IsDir() {
							err = os.RemoveAll(file)
							if err != nil {
								return errors.Wrap(err, "error in removing dir")
							}
						} else {
							if err := os.Remove(file); err != nil {
								return errors.Wrap(err, "error in removing file")
							}
						}
					}
					upVersions = append(upVersions, version)
				}
			}
			ec.Spin("Removing versions from database...")
			migrateDrv, err := newMigrate(ec, true)
			if err != nil {
				return errors.Wrap(err, "unable to initialize migrations driver")
			}
			err = migrateDrv.RemoveVersions(upVersions)
			if err != nil {
				return errors.Wrap(err, "unable to remove versions from database")
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
				return errors.Wrap(err, "cannot validate new config")
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
			migrateDrv, err = newMigrate(ec, true)
			if err != nil {
				return errors.Wrap(err, "unable to initialize migrations driver")
			}
			files, err := migrateDrv.ExportMetadata()
			if err != nil {
				return errors.Wrap(err, "cannot export metadata from server")
			}
			ec.Spin("Writing metadata...")
			err = migrateDrv.WriteMetadata(files)
			if err != nil {
				return errors.Wrap(err, "cannot write metadata")
			}
			ec.Spin("Writing new config file...")
			err = ec.WriteConfig(nil)
			if err != nil {
				return errors.Wrap(err, "cannot write config file")
			}
			ec.Spinner.Stop()
			ec.Logger.Infoln("Updated config to version 2")
			return nil
		},
	}

	f := scriptsUpdateConfigV2Cmd.Flags()
	f.StringVar(&metadataDir, "metadata-dir", "metadata", "")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return scriptsUpdateConfigV2Cmd
}
