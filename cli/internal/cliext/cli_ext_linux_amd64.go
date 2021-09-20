// +build linux
// +build amd64

package cliext

import "embed"

//go:embed static-bin/linux/amd64/*
var cliExtFS embed.FS
