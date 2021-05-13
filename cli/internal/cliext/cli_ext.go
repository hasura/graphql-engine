package cliext

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"

	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
)

// dirPath returns the directory path in which cli-ext binary will be present
func dirPath(ec *cli.ExecutionContext) string {
	return filepath.Join(ec.GlobalConfigDir, "cli-ext", ec.Version.GetCLIVersion())
}

// BinPath returns the full path of the cli-ext binary
func BinPath(ec *cli.ExecutionContext) string {
	cliExtBinName := "cli-ext"
	if runtime.GOOS == "windows" {
		cliExtBinName = "cli-ext.exe"
	}
	return filepath.Join(dirPath(ec), cliExtBinName)
}

// Setup sets up cli-ext binary for using it in various cli commands
func Setup(ec *cli.ExecutionContext) error {
	cliExtDirPath := dirPath(ec)

	err := os.MkdirAll(cliExtDirPath, 0755)
	if err != nil {
		return errors.Wrapf(err, "error creating directory while setting up cli-ext")
	}

	err = ioutil.WriteFile(BinPath(ec), cliExtFile, 0755)
	if err != nil {
		return errors.Wrap(err, "error unpacking binary while setting up cli-ext")
	}

	return nil
}
