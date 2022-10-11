// +build darwin
// +build arm64

package cliext

import "embed"

//go:embed static-bin/darwin/arm64/*
var cliExtFS embed.FS
