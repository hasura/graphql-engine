package cliext

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"runtime"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func getCliExtFileContent(ec *cli.ExecutionContext) ([]byte, error) {
	var op errors.Op = "cliext.getCliExtFileContent"
	if ec.CliExtSourceBinPath != "" {
		ec.Logger.Debug("cli-ext: setting up using --cli-ext-path binary")
		b, err := ioutil.ReadFile(ec.CliExtSourceBinPath)
		if err != nil {
			return b, errors.E(op, err)
		}
		return b, nil
	}

	var cliExtBinName string = "cli-ext"
	if runtime.GOOS == "windows" {
		cliExtBinName = "cli-ext.exe"
	}
	cliExtFileContent, err := cliExtFS.ReadFile(filepath.ToSlash(filepath.Join("static-bin", runtime.GOOS, runtime.GOARCH, cliExtBinName)))
	if err == nil {
		ec.Logger.Debug("cli-ext: setting up using cli-ext embedded in cli binary")
		return cliExtFileContent, nil
	}

	cliExtVersion := "v2.4.0-beta.3"
	ec.Logger.Warn("Unable to find an embedded cli-ext. So trying to fetch it from CDN")
	ec.Logger.Warn("Tip: --cli-ext-path can be used for setting up cli-ext from local file system")
	CDNpath := fmt.Sprintf("https://graphql-engine-cdn.hasura.io/cli-ext/releases/versioned/%s/cli-ext-%s-%s",
		cliExtVersion,
		runtime.GOOS,
		runtime.GOARCH,
	)
	resp, err := http.Get(CDNpath)
	if err != nil {
		return nil, errors.E(op, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, errors.E(op, fmt.Errorf("error downloading cli-ext from CDN %s with status code: %d", CDNpath, resp.StatusCode))
	}
	b, err := ioutil.ReadAll(bufio.NewReader(resp.Body))
	if err != nil {
		return b, errors.E(op, err)
	}
	return b, nil
}

// Setup sets up cli-ext binary for using it in various cli commands
func Setup(ec *cli.ExecutionContext) error {
	var op errors.Op = "cliext.Setup"
	parentDirPath := filepath.Join(ec.GlobalConfigDir, "cli-ext", ec.Version.GetCLIVersion())
	err := os.MkdirAll(parentDirPath, 0755)
	if err != nil {
		return errors.E(op, fmt.Errorf("error creating base directory while setting up cli-ext: %w", err))
	}
	cliExtDirPath, err := ioutil.TempDir(parentDirPath, "cli-ext-*")
	if err != nil {
		return errors.E(op, fmt.Errorf("error creating directory while setting up cli-ext: %w", err))
	}
	ec.CliExtDestinationDir = cliExtDirPath

	cliExtBinName := "cli-ext"
	if runtime.GOOS == "windows" {
		cliExtBinName = "cli-ext.exe"
	}
	cliExtBinPath := filepath.Join(cliExtDirPath, cliExtBinName)
	cliExtFileContent, err := getCliExtFileContent(ec)
	if err != nil {
		return errors.E(op, fmt.Errorf("error reading cli-ext binary: %w", err))
	}
	err = ioutil.WriteFile(cliExtBinPath, cliExtFileContent, 0755)
	if err != nil {
		return errors.E(op, fmt.Errorf("error unpacking binary while setting up cli-ext: %w", err))
	}
	ec.CliExtDestinationBinPath = cliExtBinPath

	return nil
}

func Cleanup(ec *cli.ExecutionContext) {
	_ = os.RemoveAll(ec.CliExtDestinationDir)
}
