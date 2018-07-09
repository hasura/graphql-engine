package commands

import (
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util/fake"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/sirupsen/logrus/hooks/test"
)

func TestConsoleCmd(t *testing.T) {
	logger, _ := test.NewNullLogger()
	opts := &consoleOptions{
		EC: &cli.ExecutionContext{
			Logger:  logger,
			Spinner: spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:  "http://localhost:8080",
				AccessKey: "",
			},
			Version: version.New(),
		},
		APIPort:         "9693",
		ConsolePort:     "9695",
		Address:         "localhost",
		DontOpenBrowser: true,
	}
	opts.EC.Spinner.Writer = &fake.FakeWriter{}
	err := opts.EC.Config.ParseEndpoint()
	if err != nil {
		t.Fatal(err)
	}

	go func() {
		t.Log("waiting for console to start")
		for opts.WG == nil {
			time.Sleep(1 * time.Second)
		}
		opts.WG.Done()
		opts.WG.Done()
	}()
	err = opts.run()
	if err != nil {
		t.Fatalf("failed running console: %v", err)
	}
	// TODO: (shahidhk) curl the console endpoint for 200 response
}
