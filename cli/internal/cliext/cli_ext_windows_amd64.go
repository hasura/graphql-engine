//go:build windows && amd64

package cliext

import "embed"

//go:embed static-bin/windows/amd64/*
var cliExtFS embed.FS
