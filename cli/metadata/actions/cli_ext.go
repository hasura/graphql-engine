//+build !test_plugins

package actions

import "os/exec"

func getCLIExtPath(cmdName string) (*exec.Cmd, error) {
	return exec.Command(cmdName, "cli-ext"), nil
}
