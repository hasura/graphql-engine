// +build linux
// +build arm64

package cliext

import "embed"

//go:embed static-bin/linux/arm64/*
var cliExtFS embed.FS
