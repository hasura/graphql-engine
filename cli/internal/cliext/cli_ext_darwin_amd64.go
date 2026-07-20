//go:build darwin && amd64

package cliext

import "embed"

//go:embed static-bin/darwin/amd64/*
var cliExtFS embed.FS
