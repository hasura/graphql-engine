package scripts

import (
	"path/filepath"
	"regexp"

	"github.com/hasura/graphql-engine/cli"

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
	MigrationsAbsDirectoryPath string
	SeedsAbsDirectoryPath      string
	Logger                     *logrus.Logger
}

// UpgradeProjectToMultipleSources will help a project directory move from a single
// datasource structure to multiple datasource
// The project is expected to be in Config V2
func UpgradeProjectToMultipleSources(opts UpgradeToMuUpgradeProjectToMultipleSourcesOpts) error {
	/* New flow
	Config V2 -> Config V3
	- Warn user about creating a backup
	- Ask user for the name of datasource to migrate to
	- Move current migration directories to a new source directory
	- Move seeds belonging to the source to a new directory
	- Update config file and version
	*/

	// Validate config version is
	if opts.EC.Config.Version != cli.V2 {
		return fmt.Errorf("project should be using config V2 to be able to update to V3")
	}

	opts.Logger.Warn("The upgrade process will make some changes to your project directory, It is advised to create a backup project directory before continuing")
	response, err := util.GetYesNoPrompt("continue?")
	if err != nil {
		return err
	}
	if response == "n" {
		return nil
	}
	if err != nil {
		return err
	}
	// move migration child directories
	// get directory names to move
	targetDatasource, err := util.GetInputPrompt("what datasource does the current migrations / seeds belong to?")
	if err != nil {
		return err
	}
	// move migration child directories
	// get directory names to move
	migrationDirectoriesToMove, err := getMigrationDirectoryNames(opts.Fs, opts.MigrationsAbsDirectoryPath)
	if err != nil {
		return errors.Wrap(err, "getting list of migrations to move")
	}
	// move seed child directories
	// get directory names to move
	seedFilesToMove, err := getSeedFiles(opts.Fs, opts.SeedsAbsDirectoryPath)
	if err != nil {
		return errors.Wrap(err, "getting list of seed files to move")
	}

	// create a new directory for TargetDatasource
	targetMigrationsDirectoryName := filepath.Join(opts.MigrationsAbsDirectoryPath, targetDatasource)
	if err = opts.Fs.Mkdir(targetMigrationsDirectoryName, 0755); err != nil {
		errors.Wrap(err, "creating target datasource name")
	}

	// create a new directory for TargetDatasource
	targetSeedsDirectoryName := filepath.Join(opts.SeedsAbsDirectoryPath, targetDatasource)
	if err = opts.Fs.Mkdir(targetSeedsDirectoryName, 0755); err != nil {
		errors.Wrap(err, "creating target datasource name")
	}

	// move migration directories to target datasource directory
	if err := copyMigrations(opts.Fs, migrationDirectoriesToMove, opts.MigrationsAbsDirectoryPath, targetMigrationsDirectoryName); err != nil {
		return errors.Wrap(err, "moving migrations to target datasource directory")
	}
	// move seed directories to target datasource directory
	if err := copyFiles(opts.Fs, seedFilesToMove, opts.SeedsAbsDirectoryPath, targetSeedsDirectoryName); err != nil {
		return errors.Wrap(err, "moving seeds to target datasource directory")
	}

	// write new config file
	newConfig := *opts.EC.Config
	newConfig.Version = cli.V3
	newConfig.DatasourcesConfig = []cli.DatasourceConfig{
		{
			Name:                targetDatasource,
			MigrationsDirectory: targetDatasource,
			SeedsDirectory:      targetDatasource,
		},
	}
	newConfig.ServerConfig.APIPaths.SetDefaults(opts.EC.HasMetadataV3, opts.EC.Config.Version)
	if err := opts.EC.WriteConfig(&newConfig); err != nil {
		return err
	}

	// delete original migrations
	if err := removeDirectories(opts.Fs, opts.MigrationsAbsDirectoryPath, migrationDirectoriesToMove); err != nil {
		return errors.Wrap(err, "removing up original migrations")
	}
	// delete original seeds
	if err := removeDirectories(opts.Fs, opts.SeedsAbsDirectoryPath, seedFilesToMove); err != nil {
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

func copyMigrations(fs afero.Fs, dirs []string, parentDir, target string) error {
	for _, dir := range dirs {
		f, _ := fs.Stat(filepath.Join(parentDir, dir))
		if f != nil {
			if f.IsDir() {
				err := util.CopyDirAfero(fs, filepath.Join(parentDir, dir), filepath.Join(target, dir))
				if err != nil {
					return errors.Wrapf(err, "moving %s to %s", dir, target)
				}
			}else{
				err := util.CopyFileAfero(fs, filepath.Join(parentDir, dir), filepath.Join(target, dir))
				if err != nil {
					return errors.Wrapf(err, "moving %s to %s", dir, target)
				}
			}
		}

	}
	return nil
}

func copyFiles(fs afero.Fs, files []string, parentDir, target string) error {
	for _, dir := range files {
		err := util.CopyFileAfero(fs, filepath.Join(parentDir, dir), filepath.Join(target, dir))
		if err != nil {
			return errors.Wrapf(err, "moving %s to %s", dir, target)
		}
	}
	return nil
}

func getMigrationDirectoryNames(fs afero.Fs, rootMigrationsDir string) ([]string, error) {
	return getMatchingFilesAndDirs(fs, rootMigrationsDir, isHasuraCLIGeneratedMigration)
}

func getSeedFiles(fs afero.Fs, rootSeedDir string) ([]string, error) {
	// find migrations which are in the format <timestamp>_name
	var seedFiles []string
	dirs, err := afero.ReadDir(fs, rootSeedDir)
	if err != nil {
		return nil, err
	}
	for _, info := range dirs {
		if !info.IsDir() {
			seedFiles = append(seedFiles, filepath.Join(info.Name()))
		}

	}
	return seedFiles, nil
}

func getMatchingFilesAndDirs(fs afero.Fs, parentDir string, matcher func(string) (bool, error)) ([]string, error) {
	// find migrations which are in the format <timestamp>_name
	var migrations []string
	dirs, err := afero.ReadDir(fs, parentDir)
	if err != nil {
		return nil, err
	}
	for _, info := range dirs {
		if ok, err := matcher(info.Name()); !ok || err != nil {
			if err != nil {
					  return nil, err
					  }
			continue
		}
		migrations = append(migrations, filepath.Join(info.Name()))

	}
	return migrations, nil
}

func isHasuraCLIGeneratedMigration(dirPath string) (bool, error) {
	const regex = `^([0-9]{13})_(.*)$`
	return regexp.MatchString(regex, filepath.Base(dirPath))
}
