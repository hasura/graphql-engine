package main

import (
	"github.com/hasura/graphql-engine/cli/commands"
	log "github.com/sirupsen/logrus"
)

func main() {
	err := commands.Execute()
	if err != nil {
		log.Fatal(err)
	}
}
