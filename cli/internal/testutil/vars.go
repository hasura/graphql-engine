package testutil

import (
	"fmt"
	"os"
	"runtime"
)

// this can be overridden by ldflags
var (
	HasuraVersion  = "main-225403eda"
	DockerSwitchIP = func() string {
		switch runtime.GOOS {
		case "darwin", "windows":
			return "host.docker.internal"
		default:
			return "172.17.0.1"
		}
		return ""
	}()
	Hostname        = "localhost"
	BaseURL         = fmt.Sprintf("http://%s", Hostname)
	MSSQLPassword   = "MSSQLp@ssw0rd"
	SkipDockerTests = func() bool {
		if len(os.Getenv("CI")) > 0 {
			// skip in CI
			return true
		}
		return false
	}()
	CLIBinaryPath = "hasura"
)
