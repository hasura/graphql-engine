// Package main is the entrypoint for the command line executable.
package main

import (
	"github.com/hasura/graphql-engine/cli/commands"
	log "github.com/sirupsen/logrus"
)

// main is the entrypoint function
func main() {
	err := commands.Execute()
	if commands.EC.Spinner != nil {
		commands.EC.Spinner.Stop()
	}
	if err != nil {
		if commands.EC.Telemetry != nil {
			commands.EC.Telemetry.IsError = true
			commands.EC.Telemetry.Beam()
		}
		log.Fatal(err)
	} else {
		if commands.EC.Telemetry != nil {
			commands.EC.Telemetry.Beam()
		}
	}
}
