package cli

import (
	stderrors "errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
	var op errors.Op = "cli.ExecutionContext.validateDirectory"
	var err error
	if len(ec.ExecutionDirectory) == 0 {
		cwd, err := os.Getwd()
		if err != nil {
			return errors.E(op, fmt.Errorf("error getting current working directory: %w", err))
		}
		ec.ExecutionDirectory = cwd
	} else {
		ec.ExecutionDirectory, err = filepath.Abs(ec.ExecutionDirectory)
		if err != nil {
			return errors.E(op, fmt.Errorf("error finding absolute path for project directory: %w", err))
		}
	}

	ed, err := os.Stat(ec.ExecutionDirectory)
	if err != nil {
		if stderrors.Is(err, fs.ErrNotExist) {
			return errors.E(op, fmt.Errorf("did not find required directory. use 'init'?: %w", err))
		}
		return errors.E(op, fmt.Errorf("error getting directory details: %w", err))
	}
	if !ed.IsDir() {
		return errors.E(op, fmt.Errorf("'%s' is not a directory: %w", ed.Name(), err))
	}
	// config.yaml
	// migrations/
	// (optional) metadata.yaml
	dir, err := recursivelyValidateDirectory(ec.ExecutionDirectory)
	if err != nil {
		return errors.E("validate: %w", err)
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
	var op errors.Op = "cli.recursivelyValidateDirectory"
	err = ValidateDirectory(startFrom)
	if err != nil {
		nextDir := filepath.Dir(startFrom)
		// to catch error gracefully in loop situation
		if nextDir == startFrom {
			return "", errors.E(op, fmt.Errorf("failed recursively find config.yaml: search stopped due to a possible infinite filesystem traversal at %s", nextDir))
		}
		if err := CheckFilesystemBoundary(nextDir); err != nil {
			return nextDir, errors.E(op, fmt.Errorf("cannot find [%s] | search stopped: %w", strings.Join(filesRequired, ", "), err))
		}
		return recursivelyValidateDirectory(nextDir)
	}
	return startFrom, nil
}

// validateDirectory tries to parse dir for the filesRequired and returns error
// if any one of them is missing.
func ValidateDirectory(dir string) error {
	var op errors.Op = "cli.ValidateDirectory"
	notFound := []string{}
	for _, f := range filesRequired {
		if _, err := os.Stat(filepath.Join(dir, f)); stderrors.Is(err, fs.ErrNotExist) {
			relpath, e := filepath.Rel(dir, f)
			if e == nil {
				f = relpath
			}
			notFound = append(notFound, f)
		}
	}
	if len(notFound) > 0 {
		return errors.E(op, fmt.Errorf("cannot validate directory '%s': [%s] not found", dir, strings.Join(notFound, ", ")))
	}
	return nil
}

// CheckFilesystemBiundary returns an error if dir is filesystem root
func CheckFilesystemBoundary(dir string) error {
	var op errors.Op = "cli.CheckFilesystemBoundary"
	// since filepath.Abs calls filepath.Clean the path is expected to be in "clean" state
	isWindowsRoot, _ := regexp.MatchString(`^[a-zA-Z]:\\$`, dir)
	// return error if filesystem boundary is hit
	if dir == "/" || isWindowsRoot {
		return errors.E(op, "filesystem boundary hit")

	}
	return nil
}
