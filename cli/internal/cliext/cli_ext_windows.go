// +build windows

package cliext

import _ "embed"

//go:embed bin/cli-ext-hasura-win.exe
var cliExtFile []byte
