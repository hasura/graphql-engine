package commands

import (
	"os"
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
	ec := cli.NewExecutionContext()
	ec.Telemetry.Command = "TEST"
	ec.Logger = logger
	ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
	ec.ServerConfig = &cli.ServerConfig{
		Endpoint:    "http://localhost:8080",
		AdminSecret: os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
	}
	ec.MetadataFile = []string{"metadata.yaml"}

	ec.Version = version.New()
	v, err := version.FetchServerVersion(ec.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	ec.Version.SetServerVersion(v)
	err = ec.Prepare()
	if err != nil {
		t.Fatalf("prepare failed: %v", err)
	}
	opts := &consoleOptions{
		EC:              ec,
		APIPort:         "9693",
		ConsolePort:     "9695",
		Address:         "localhost",
		DontOpenBrowser: true,
	}
	opts.EC.Spinner.Writer = &fake.FakeWriter{}
	err = opts.EC.ServerConfig.ParseEndpoint()
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
