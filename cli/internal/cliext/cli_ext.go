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
	"github.com/pkg/errors"
)

func getCliExtFileContent(ec *cli.ExecutionContext) ([]byte, error) {
	if ec.CliExtSourceBinPath != "" {
		ec.Logger.Debug("cli-ext: setting up using --cli-ext-path binary")
		return ioutil.ReadFile(ec.CliExtSourceBinPath)
	}

	cliExtFileContent, err := cliExtFS.ReadFile(filepath.Join("static-bin", runtime.GOOS, runtime.GOARCH, "cli-ext"))
	if err == nil {
		ec.Logger.Debug("cli-ext: setting up using cli-ext embedded in cli binary")
		return cliExtFileContent, nil
	}

	cliExtVersion := "v2.0.0"
	ec.Logger.Warn("Unable to find an embedded cli-ext. So trying to fetch it from CDN")
	ec.Logger.Warn("Tip: --cli-ext-path can be used for setting up cli-ext from local file system")
	CDNpath := fmt.Sprintf("https://graphql-engine-cdn.hasura.io/cli-ext/releases/versioned/%s/cli-ext-%s-%s",
		cliExtVersion,
		runtime.GOOS,
		runtime.GOARCH,
	)
	resp, err := http.Get(CDNpath)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("error downloading cli-ext from CDN %s with status code: %d", CDNpath, resp.StatusCode)
	}
	return ioutil.ReadAll(bufio.NewReader(resp.Body))
}

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
	ec.CliExtDestinationDir = cliExtDirPath

	cliExtBinName := "cli-ext"
	if runtime.GOOS == "windows" {
		cliExtBinName = "cli-ext.exe"
	}
	cliExtBinPath := filepath.Join(cliExtDirPath, cliExtBinName)
	cliExtFileContent, err := getCliExtFileContent(ec)
	if err != nil {
		return fmt.Errorf("error reading cli-ext binary: %w", err)
	}
	err = ioutil.WriteFile(cliExtBinPath, cliExtFileContent, 0755)
	if err != nil {
		return errors.Wrap(err, "error unpacking binary while setting up cli-ext")
	}
	ec.CliExtDestinationBinPath = cliExtBinPath

	return nil
}

func Cleanup(ec *cli.ExecutionContext) {
	_ = os.RemoveAll(ec.CliExtDestinationDir)
}
