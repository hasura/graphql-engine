package commands

import (
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus"
)

func TestConsoleCmd(t *testing.T) {
	opts := &consoleOptions{
		EC: &cli.ExecutionContext{
			Logger:  logrus.New(),
			Spinner: spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:  "http://localhost:8080",
				AccessKey: "",
			},
		},
		APIPort:         "9693",
		ConsolePort:     "9695",
		Address:         "localhost",
		DontOpenBrowser: true,
	}

	go func() {
		t.Log("waiting for console to start")
		for opts.WG == nil {
			time.Sleep(1 * time.Second)
		}
		opts.WG.Done()
		opts.WG.Done()
	}()
	err := opts.Run()
	if err != nil {
		t.Fatalf("failed running console: %v", err)
	}
	// TODO: (shahidhk) curl the console endpoint for 200 response
}
