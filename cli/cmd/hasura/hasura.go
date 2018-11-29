// Package main is the entrypoint for the command line executable.
package main

import (
	"github.com/hasura/graphql-engine/cli/commands"
	"github.com/hasura/graphql-engine/cli/telemetry"
	log "github.com/sirupsen/logrus"
)

// main is the entrypoint function
func main() {
	err := commands.Execute()
	if commands.EC.Spinner != nil {
		commands.EC.Spinner.Stop()
	}
	telemetry.Waiter.Wait()
	if err != nil {
		telemetry.SendErrorEvent(commands.EC, nil)
		log.Fatal(err)
	}
}
