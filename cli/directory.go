package cli

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/pkg/errors"
)

// validateDirectory sets execution directory and validate it to see that or any
// of the parent directory is a valid project directory. A valid project
// directory contains the following:
// 1. migrations directory
// 2. config.yaml file
// 3. metadata.yaml (optional)
// If the current directory or any parent directory (upto filesystem root) is
// found to have these files, ExecutionDirectory is set as that directory.
func (ec *ExecutionContext) validateDirectory() error {
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
	// config.yaml
	// migrations/
	// (optional) metadata.yaml
	dir, err := recursivelyValidateDirectory(ec.ExecutionDirectory)
	if err != nil {
		return errors.Wrap(err, "validate")
	}

	ec.ExecutionDirectory = dir
	return nil
}

// filesRequired are the files that are mandatory to qualify for a project
// directory.
var filesRequired = []string{
	"config.yaml",
}

// recursivelyValidateDirectory tries to parse 'startFrom' as a project
// directory by checking for the 'filesRequired'. If the parent of 'startFrom'
// (nextDir) is filesystem root, error is returned. Otherwise, 'nextDir' is
// validated, recursively.
func recursivelyValidateDirectory(startFrom string) (validDir string, err error) {
	err = ValidateDirectory(startFrom)
	if err != nil {
		nextDir := filepath.Dir(startFrom)
		if err := CheckFilesystemBoundary(nextDir); err != nil {
			return nextDir, errors.Wrapf(err, "cannot find [%s] | search stopped", strings.Join(filesRequired, ", "))
		}
		return recursivelyValidateDirectory(nextDir)
	}
	return startFrom, nil
}

// validateDirectory tries to parse dir for the filesRequired and returns error
// if any one of them is missing.
func ValidateDirectory(dir string) error {
	notFound := []string{}
	for _, f := range filesRequired {
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

// CheckFilesystemBiundary returns an error if dir is filesystem root
func CheckFilesystemBoundary(dir string) error {
	cleaned := filepath.Clean(dir)
	isWindowsRoot, _ := regexp.MatchString(`^[a-zA-Z]:\\$`, cleaned)
	// return error if filesystem boundary is hit
	if cleaned == "/" || isWindowsRoot {
		return errors.Errorf("filesystem boundary hit")

	}
	return nil
}
