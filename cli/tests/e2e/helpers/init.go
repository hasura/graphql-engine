package helpers

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
)

const E2eTestEnvPrefix = "HASURA_CLI_E2E_TESTS"

var (
	HGEBinaryPathENVVar = fmt.Sprintf("%s_%s", E2eTestEnvPrefix, "HGE_BINARY_PATH")
	HGEServerPathENVVar = fmt.Sprintf("%s_%s", E2eTestEnvPrefix, "HGE_SERVER_DIRECTORY_PATH")
	CLIBinaryPathEnvVar = fmt.Sprintf("%s_%s", E2eTestEnvPrefix, "CLI_BINARY_PATH")
)

var (
	hgeBinaryPath, hgeServerDirectoryPath, cliBinaryPath string
)

func init() {
	var err error
	// get value from env vars if set
	hgeBinaryPath = os.Getenv(HGEBinaryPathENVVar)
	if v := os.Getenv(HGEServerPathENVVar); v != "" {
		hgeServerDirectoryPath, err = filepath.Abs(v)
		if err != nil {
			log.Fatal(err)
		}
	}
	cliBinaryPath = os.Getenv(CLIBinaryPathEnvVar)
}
