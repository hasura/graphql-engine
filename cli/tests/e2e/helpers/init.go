package helpers

import (
	"fmt"
	"os"
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
	// get value from env vars if set
	hgeBinaryPath = os.Getenv(HGEBinaryPathENVVar)
	hgeServerDirectoryPath = os.Getenv(HGEServerPathENVVar)
	cliBinaryPath = os.Getenv(CLIBinaryPathEnvVar)
}
