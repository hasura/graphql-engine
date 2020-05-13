package cli

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/pkg/errors"
)

// validateDirectory sets execution directory and the optional config file.
// It checks for a config file in cwd (or directory provided in project flag)
// or any of the parent directory. If a config file is found, ExecutionDirectory
// and ConfigFile is set.
// If in the current directory or any parent directory (upto filesystem root)
// no configuration file exists, ExecutionDirectory is set to current working
// directory (or directory provided in project flag) and ConfigFile is an empty string.
func (ec *ExecutionContext) setExecutionDirectory() error {
	// when project flag is not set, set cwd as execution directory
	if len(ec.ExecutionDirectory) == 0 {
		cwd, err := os.Getwd()
		if err != nil {
			return errors.Wrap(err, "error getting current working directory")
		}
		ec.ExecutionDirectory = cwd
	}

	ed, err := os.Stat(ec.ExecutionDirectory)
	if err != nil {
		if os.IsNotExist(err) {
			return errors.Wrap(err, "did not find required directory. use 'init'?")
		}
		return errors.Wrap(err, "error getting directory details")
	}
	if !ed.IsDir() {
		return errors.Errorf("'%s' is not a directory", ed.Name())
	}

	// check for configuration file
	dir, err := recursivelyFindConfig(ec.ExecutionDirectory)
	if err != nil {
		ec.Logger.Debug("No config file found, execution directory is ", ec.ExecutionDirectory)
		return nil
	} else {
		ec.ExecutionDirectory = dir
		ec.ConfigFile = filepath.Join(ec.ExecutionDirectory, "config.yaml")
	}
	return nil
}

// filesRequiredV1 are the files that are mandatory to qualify for a project
// directory with config V1.
var filesRequiredV1 = []string{
	"config.yaml",
	"migrations",
}

var ConfigFile = []string{
	"config.yaml",
}

// recursivelyFindConfig tries to parse 'startFrom' as a project
// directory by checking for the 'configFile'. If the parent of 'startFrom'
// (nextDir) is filesystem root, error is returned. Otherwise, 'nextDir' is
// validated, recursively.
func recursivelyFindConfig(startFrom string) (validDir string, err error) {
	err = ValidateDirectory(startFrom, ConfigFile)
	if err != nil {
		nextDir := filepath.Dir(startFrom)
		if err := CheckFilesystemBoundary(nextDir); err != nil {
			return nextDir, errors.Wrapf(err, "cannot find [%s] | search stopped", strings.Join(ConfigFile, ", "))
		}
		return recursivelyFindConfig(nextDir)
	}
	return startFrom, nil
}

// ValidateDirectory tries to parse dir for the 'filesToCheck' and returns error
// if any one of them is missing.
func ValidateDirectory(dir string, filesToCheck []string) error {
	notFound := []string{}
	for _, f := range filesToCheck {
		if _, err := os.Stat(filepath.Join(dir, f)); os.IsNotExist(err) {
			relpath, e := filepath.Rel(dir, f)
			if e == nil {
				f = relpath
			}
			notFound = append(notFound, f)
		}
	}
	if len(notFound) > 0 {
		return errors.Errorf("cannot validate directory '%s': [%s] not found", dir, strings.Join(notFound, ", "))
	}
	return nil
}

// CheckFilesystemBoundary returns an error if dir is filesystem root
func CheckFilesystemBoundary(dir string) error {
	cleaned := filepath.Clean(dir)
	isWindowsRoot, _ := regexp.MatchString(`^[a-zA-Z]:\\$`, cleaned)
	// return error if filesystem boundary is hit
	if cleaned == "/" || isWindowsRoot {
		return errors.Errorf("filesystem boundary hit")

	}
	return nil
}

// validateV1Directory checks version of config file. If the
// version is V1, execution directory is strictly validated.
// A valid V1 project directory contains the following:
// 1. migrations directory
// 2. config.yaml file
// 3. metadata.yaml (optional)
func (ec *ExecutionContext) validateV1Directory() error {
	// Strict directory check for version 1 config.
	if ec.Config.Version == V1 {
		if err := ValidateDirectory(ec.ExecutionDirectory, filesRequiredV1); err != nil {
			return err
		}
	}
	return nil
}
