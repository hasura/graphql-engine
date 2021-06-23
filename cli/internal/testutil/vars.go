package testutil

import (
	"fmt"
	"os"
	"runtime"
)

// this can be overridden by ldflags
var (
	HasuraDockerImage = func() string {
		graphqlEngineDockerImage := os.Getenv("HASURA_TEST_CLI_HGE_DOCKER_IMAGE")
		if graphqlEngineDockerImage != "" {
			return graphqlEngineDockerImage
		}
		return ""
	}()

	TestAdminSecret = os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")

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
