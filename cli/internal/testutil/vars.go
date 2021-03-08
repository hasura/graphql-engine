package testutil

import (
	"fmt"
	"os"
)

// this can be overridden by ldflags
var (
	HasuraVersion   = "v2.0.0-alpha.2"
	DockerSwitchIP  = "172.17.0.1"
	Hostname        = "localhost"
	BaseURL         = fmt.Sprintf("http://%s", Hostname)
	MSSQLPassword   = "MSSQLp@ssw0rd"
	SkipDockerTests = func() string {
		if len(os.Getenv("CI")) > 0 {
			// skip in CI
			return "true"
		}
		return "false"
	}()
)
