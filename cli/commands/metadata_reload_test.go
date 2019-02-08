package commands

import (
	"net/url"
	"os"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/sirupsen/logrus/hooks/test"
)

func testMetadataReload(t *testing.T, metadataFile string, endpoint *url.URL) {
	logger, _ := test.NewNullLogger()
	opts := &metadataReloadOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: metadataFile,
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpoint.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpoint,
			},
		},
		actionType: "reload",
	}

	opts.EC.Version = version.New()
	v, err := version.FetchServerVersion(opts.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	opts.EC.Version.SetServerVersion(v)

	err = opts.run()
	if err != nil {
		t.Fatalf("failed reloading metadata: %v", err)
	}
}
