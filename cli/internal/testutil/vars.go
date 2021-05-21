package testutil

import (
	"fmt"
	"os"
	"runtime"
)

// this can be overridden by ldflags
var (
	HasuraVersion = func() string {
		graphqlEngineDockerTag := os.Getenv("HASURA_TEST_CLI_HGE_DOCKER_TAG")
		if graphqlEngineDockerTag != "" {
			return graphqlEngineDockerTag
		}

		return ""
	}()
	HasuraDockerRepo = func() string {
		graphqlEngineDockerTag := os.Getenv("HASURA_TEST_CLI_HGE_DOCKER_REPO")
		if graphqlEngineDockerTag != "" {
			return graphqlEngineDockerTag
		}
		return "hasura/graphql-engine"
	}()
	DockerSwitchIP = func() string {
		switch runtime.GOOS {
		case "darwin", "windows":
			return "host.docker.internal"
		}
		return "172.17.0.1"
	}()
	Hostname      = "localhost"
	BaseURL       = fmt.Sprintf("http://%s", Hostname)
	MSSQLPassword = "MSSQLp@ssw0rd"
	CLIBinaryPath = func() string {
		if os.Getenv("CI") == "true" {
			return "/build/_cli_output/binaries/cli-hasura-linux-amd64"
		}

		hasuraCliPathEnv := os.Getenv("HASURA_TEST_CLI_PATH")
		if hasuraCliPathEnv != "" {
			return hasuraCliPathEnv
		}

		return "hasura"
	}()
)
