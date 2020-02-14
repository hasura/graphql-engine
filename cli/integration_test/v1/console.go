package v1

import (
	"os"
	"testing"
	"time"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/commands"
)

func TestConsoleCmd(t *testing.T, ec *cli.ExecutionContext) {
	opts := &commands.ConsoleOptions{
		EC:                  ec,
		APIPort:             "9693",
		ConsolePort:         "9695",
		Address:             "localhost",
		DontOpenBrowser:     true,
		APIServerSignal:     make(chan os.Signal),
		ConsoleServerSignal: make(chan os.Signal),
	}

	go func() {
		t.Log("waiting for console to start")
		for opts.WG == nil {
			time.Sleep(1 * time.Second)
		}
		opts.APIServerSignal <- os.Interrupt
		opts.ConsoleServerSignal <- os.Interrupt
		close(opts.APIServerSignal)
		close(opts.ConsoleServerSignal)
	}()
	err := opts.Run()
	if err != nil {
		t.Fatalf("failed running console: %v", err)
	}
	// TODO: (shahidhk) curl the console endpoint for 200 response
}
