//+build test_plugins

package actions

import (
	"os/exec"
	"os/user"
	"path/filepath"
)

func getCLIExtPath(_ string) (*exec.Cmd, error) {
	usr, err := user.Current()
	if err != nil {
		return nil, err
	}
	return exec.Command(filepath.Join(usr.HomeDir, ".hasura/plugins/bin", "hasura-cli_ext")), nil
}
