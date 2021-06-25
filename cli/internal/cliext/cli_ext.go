package cliext

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/pkg/errors"
)

// Setup sets up cli-ext binary for using it in various cli commands
func Setup(ec *cli.ExecutionContext) error {
	parentDirPath := filepath.Join(ec.GlobalConfigDir, "cli-ext", ec.Version.GetCLIVersion())
	err := os.MkdirAll(parentDirPath, 0755)
	if err != nil {
		return errors.Wrapf(err, "error creating base directory while setting up cli-ext")
	}

	cliExtDirPath, err := ioutil.TempDir(parentDirPath, "cli-ext-*")
	if err != nil {
		return errors.Wrapf(err, "error creating directory while setting up cli-ext")
	}
	ec.CliExtDir = cliExtDirPath

	cliExtBinName := "cli-ext"
	if runtime.GOOS == "windows" {
		cliExtBinName = "cli-ext.exe"
	}

	cliExtBinPath := filepath.Join(cliExtDirPath, cliExtBinName)
	err = ioutil.WriteFile(cliExtBinPath, cliExtFile, 0755)
	if err != nil {
		return errors.Wrap(err, "error unpacking binary while setting up cli-ext")
	}
	ec.CliExtBinPath = cliExtBinPath

	return nil
}

func Cleanup(ec *cli.ExecutionContext) {
	_ = os.RemoveAll(ec.CliExtDir)
}
