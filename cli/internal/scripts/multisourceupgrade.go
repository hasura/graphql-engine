package scripts

import (
	"path/filepath"
	"regexp"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/client"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"

	"fmt"

	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/afero"
)

type UpgradeToMuUpgradeProjectToMultipleSourcesOpts struct {
	EC *cli.ExecutionContext
	Fs afero.Fs
	// Path to project directory
	ProjectDirectory string
	// Directory in which migrations are stored
	MigrationsDirectory string
	Logger              *logrus.Logger
}

// UpgradeProjectToMultipleSources will help a project directory move from a single
// datasource structure to multiple datasource
// The project is expected to be in Config V2
func UpgradeProjectToMultipleSources(opts UpgradeToMuUpgradeProjectToMultipleSourcesOpts) error {
	hasuraAPIClient, err := client.NewHasuraRestAPIClient(client.NewHasuraRestAPIClientOpts{
		Headers:        opts.EC.HGEHeaders,
		QueryAPIURL:    fmt.Sprintf("%s/%s", opts.EC.Config.Endpoint, "v2/query"),
		MetadataAPIURL: fmt.Sprintf("%s/%s", opts.EC.Config.Endpoint, "v1/metadata"),
		TLSConfig:      opts.EC.Config.TLSConfig,
	})

	if err != nil {
		return err
	}
	//get the list of data sources
	datasourcesNameAndType, err := hasuraAPIClient.GetDatasources()
	if err != nil {
		return err
	}
	var datasourceNames []string
	var datasourceConfigs []cli.DatasourceConfig
	for sourceName, sourceType := range datasourcesNameAndType {
		ds := cli.DatasourceConfig{
			Name:                sourceName,
			Type:                sourceType,
			MigrationsDirectory: filepath.Join(filepath.Dir(opts.EC.MigrationDir), sourceName),
		}
		datasourceNames = append(datasourceNames, sourceName)
		datasourceConfigs = append(datasourceConfigs, ds)
	}
	fmt.Println(datasourceNames)

	targetDatasource, err := util.GetSelectPrompt("select datasource for which current migrations belong to", datasourceNames)
	if err != nil {
		return err
	}
	// create backup of project
	var projectBackupPath = fmt.Sprintf("%s_%s", opts.ProjectDirectory, "backup")
	if err := util.CopyDirAfero(opts.Fs, opts.ProjectDirectory, projectBackupPath); err != nil {
		return errors.Wrap(err, "creating project backup directory")
	}
	opts.Logger.Debugf("created project backup at %s", projectBackupPath)

	// get directory names to move
	directoriesToMove, err := getMigrationDirectoryNames(opts.Fs, opts.MigrationsDirectory)
	if err != nil {
		return errors.Wrap(err, "getting list of migrations to move")
	}
	// create a new directory for TargetDatasource
	targetDatasourceDirectoryName := filepath.Join(opts.MigrationsDirectory, targetDatasource)
	if err = opts.Fs.Mkdir(targetDatasourceDirectoryName, 0755); err != nil {
		errors.Wrap(err, "creating target datasource name")
	}

	// move migration directories to target datasource directory
	if err := moveMigrationsToDatasourceDirectory(opts.Fs, directoriesToMove, opts.MigrationsDirectory, targetDatasourceDirectoryName); err != nil {
		return errors.Wrap(err, "moving migrations to target datasource directory")
	}
	// create new directories for all other datasources
	for _, source := range datasourceNames {
		if err := opts.Fs.MkdirAll(filepath.Join(opts.MigrationsDirectory, source), 0755); err != nil {
			return err
		}
	}

	migrations, err := hasuraAPIClient.GetMigrationVersions(hasuradb.DefaultSchema, hasuradb.DefaultMigrationsTable)
	if err != nil {
		return err
	}
	settings, err := hasuraAPIClient.GetCLISettingsFromSQLTable(hasuradb.DefaultSchema, hasuradb.DefaultSettingsTable)
	if err != nil {
		return err
	}
	if err := hasuraAPIClient.MoveMigrationsAndSettingsToCatalogState(targetDatasource, migrations, settings); err != nil {
		return err
	}

	// write new config file
	newConfig := *opts.EC.Config
	newConfig.Version = cli.V3
	newConfig.DatasourcesConfig = datasourceConfigs
	newConfig.ServerConfig.APIPaths.SetDefaults(opts.EC.Version.ServerFeatureFlags, opts.EC.Config.Version)
	if err := opts.EC.WriteConfig(&newConfig); err != nil {
		return err
	}

	// delete original migrations
	if err := removeDirectories(opts.Fs, opts.MigrationsDirectory, directoriesToMove); err != nil {
		return errors.Wrap(err, "removing up original migrations")
	}

	return nil
}

func removeDirectories(fs afero.Fs, parentDirectory string, dirNames []string) error {
	for _, d := range dirNames {
		if err := fs.RemoveAll(filepath.Join(parentDirectory, d)); err != nil {
			return err
		}
	}
	return nil
}

func moveMigrationsToDatasourceDirectory(fs afero.Fs, dirs []string, parentMigrationsDirectory, target string) error {
	for _, dir := range dirs {
		err := util.CopyDirAfero(fs, filepath.Join(parentMigrationsDirectory, dir), filepath.Join(target, dir))
		if err != nil {
			return errors.Wrapf(err, "moving %s to %s", dir, target)
		}
	}
	return nil
}
func getMigrationDirectoryNames(fs afero.Fs, rootMigrationsDir string) ([]string, error) {
	// find migrations which are in the format <timestamp>_name
	var migrationDirectories []string
	dirs, err := afero.ReadDir(fs, rootMigrationsDir)
	if err != nil {
		return nil, err
	}
	for _, info := range dirs {
		if info.IsDir() {
			if ok, err := checkIfDirectoryIsMigration(info.Name()); !ok || err != nil {
				if err != nil {
					return nil, err
				}
				continue
			}
			migrationDirectories = append(migrationDirectories, filepath.Join(info.Name()))
		}

	}
	return migrationDirectories, nil
}
func checkIfDirectoryIsMigration(dirPath string) (bool, error) {
	const regex = `^([0-9]{13})_(.*)$`
	return regexp.MatchString(regex, filepath.Base(dirPath))
}
