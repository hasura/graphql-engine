package scripts

import (
	"path/filepath"
	"regexp"

	"fmt"

	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/afero"
)

type UpgradeToMuUpgradeProjectToMultipleSourcesOpts struct {
	Fs afero.Fs
	// Path to project directory
	ProjectDirectory string
	// Directory in which migrations are stored
	MigrationsDirectory string
	// Name of the datasource which the current migrations belong to
	TargetDatasourceName string
	Logger               *logrus.Logger
}

// UpgradeProjectToMultipleSources will help a project directory move from a single
// datasource structure to multiple datasource
// The project is expected to be in Config V2
func UpgradeProjectToMultipleSources(opts UpgradeToMuUpgradeProjectToMultipleSourcesOpts) error {
	// create backup of project
	var projectBackupPath = fmt.Sprintf("%s_%s", opts.ProjectDirectory, "backup")
	if err := util.CopyDirAfero(opts.Fs, opts.ProjectDirectory, projectBackupPath); err != nil {
		return errors.Wrap(err, "creating project backup directory")
	}
	opts.Logger.Debugf("created project backup at %s", projectBackupPath)

	// get directory names to move
	directoriesToMove, err := getMigrationDirectories(opts.Fs, opts.MigrationsDirectory)
	if err != nil {
		return errors.Wrap(err, "getting list of migrations to move")
	}
	// create a new directory for TargetDatasource
	targetDatasourceDirectoryName := filepath.Join(opts.MigrationsDirectory, opts.TargetDatasourceName)
	if err = opts.Fs.Mkdir(targetDatasourceDirectoryName, 0755); err != nil {
		errors.Wrap(err, "creating target datasource name")
	}

	// move migration directories to target datasource directory
	if err := moveMigrationsToDatasourceDirectory(opts.Fs, directoriesToMove, targetDatasourceDirectoryName); err != nil {
		return errors.Wrap(err, "moving migrations to target datasource directory")
	}

	// delete original migrations
	if err := removeDirectories(opts.Fs, directoriesToMove); err != nil {
		return errors.Wrap(err, "removing up original migrations")
	}

	return nil
}

func removeDirectories(fs afero.Fs, dirs []string) error {
	for _, d := range dirs {
		if err := fs.RemoveAll(d); err != nil {
			return err
		}
	}
	return nil
}

func moveMigrationsToDatasourceDirectory(fs afero.Fs, dirs []string, target string) error {
	for _, dir := range dirs {
		err := util.CopyDirAfero(fs, dir, filepath.Join(target, dir))
		if err != nil {
			return errors.Wrapf(err, "moving %s to %s", dir, target)
		}
	}
	return nil
}
func getMigrationDirectories(fs afero.Fs, rootMigrationsDir string) ([]string, error) {
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
			migrationDirectories = append(migrationDirectories, filepath.Join(rootMigrationsDir, info.Name()))
		}

	}
	return migrationDirectories, nil
}
func checkIfDirectoryIsMigration(dirPath string) (bool, error) {
	const regex = `^([0-9]{13})_(.*)$`
	return regexp.MatchString(regex, filepath.Base(dirPath))
}
