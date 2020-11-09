package scripts

import (
	"fmt"
	"github.com/hasura/graphql-engine/cli/migrate/source/file"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"os"
	"path/filepath"
	"regexp"
)

type UpgradeToMuUpgradeProjectToMultipleSourcesOpts struct {
	Fs afero.Fs
	// Path to project directory
	ProjectDirectory string
	// Directory in which migrations are stored (relative to project directory)
	MigrationsDirectory string
	// List of datasources avialable
	DatasourceNames func() string
	// Name of the datasource which the current migrations belong to
	TargetDatasourceName func() string
	Logger logrus.Logger
}
// UpgradeProjectToMultipleSources will help a project directory move from a single
// datasource structure to multiple datasource
// The project is expected to be in Config V2
func UpgradeProjectToMultipleSources(opts UpgradeToMuUpgradeProjectToMultipleSourcesOpts) error {
	// create backup of project
	var projectBackupPath = filepath.Join(opts.ProjectDirectory, fmt.Sprintf(opts.ProjectDirectory, "_backup"))
    if err := util.CopyDirAfero(opts.Fs, opts.ProjectDirectory, projectBackupPath); err != nil {
    	return errors.Wrap(err, "creating project backup directory")
	}
	opts.Logger.Debugf("created project backup at %s", projectBackupPath)


	// create a new directory for TargetDatasource
	// move directories
	return nil
}

func moveMigrationsToDatasourceDirectory(file []file.File, target string) error {
	return nil
}
func getMigrationDirectories(fs afero.Fs, rootMigrationsDir string) ([]string,error) {
	// find migrations which are in the format <timestamp>_name
	var migrationDirectories []string
	err := afero.Walk(fs, rootMigrationsDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return errors.Wrap(err, "walking migrations directory")
		}
		if info.IsDir() && info.Name() != rootMigrationsDir {
			if ok, err := checkIfDirectoryIsMigration(info.Name()); !ok || err != nil {
				if err != nil {
					return err
				}
				return filepath.SkipDir
			}
			migrationDirectories = append(migrationDirectories, info.Name())
		}
		return nil
	})
	if err != nil {
		errors.Wrapf(err, "walking the path %q", rootMigrationsDir)
		return nil, err
	}
	return migrationDirectories, nil
}
func checkIfDirectoryIsMigration(dirPath string) (bool,error) {
	const regex = `^([0-9]{13})_(.*)$`
	return regexp.MatchString(regex, filepath.Base(dirPath))
}
