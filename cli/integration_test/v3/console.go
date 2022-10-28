package v3

import (
	"fmt"
	"net"
	"net/url"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/avast/retry-go"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/stretchr/testify/require"
)

func TestConsoleCmd(t *testing.T, ec *cli.ExecutionContext) {
	apiHost := &url.URL{
		Host:   "localhost",
		Scheme: "http",
	}
	opts := &commands.ConsoleOptions{
		EC:                           ec,
		APIPort:                      "9693",
		APIHost:                      apiHost,
		ConsolePort:                  "9695",
		Address:                      "localhost",
		DontOpenBrowser:              true,
		APIServerInterruptSignal:     make(chan os.Signal),
		ConsoleServerInterruptSignal: make(chan os.Signal),
	}

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		t.Log("waiting for console to start")
		timeout := 1 * time.Minute
		require.NoError(t, retry.Do(
			func() error {
				_, err := net.DialTimeout("tcp", fmt.Sprintf("%s:%s", opts.Address, opts.APIPort), timeout)
				return err
			}, retry.Attempts(5),
		))
		require.NoError(t, retry.Do(
			func() error {
				_, err := net.DialTimeout("tcp", fmt.Sprintf("%s:%s", opts.Address, opts.ConsolePort), timeout)
				return err
			}, retry.Attempts(5),
		))
		opts.APIServerInterruptSignal <- os.Interrupt
		opts.ConsoleServerInterruptSignal <- os.Interrupt
		close(opts.APIServerInterruptSignal)
		close(opts.ConsoleServerInterruptSignal)
	}()
	err := opts.Run()
	if err != nil {
		t.Fatalf("failed running console: %v", err)
	}

	wg.Wait()
	// TODO: (shahidhk) curl the console endpoint for 200 response
}
