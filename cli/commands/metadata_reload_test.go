package commands

import (
	"net/url"
	"os"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus/hooks/test"
)

func testMetadataReload(t *testing.T, metadataFile string, endpoint *url.URL) {
	logger, _ := test.NewNullLogger()
	opts := &metadataReloadOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: metadataFile,
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:       endpoint.String(),
				AccessKey:      os.Getenv("HASURA_GRAPHQL_TEST_ACCESS_KEY"),
				ParsedEndpoint: endpoint,
			},
		},
		actionType: "reload",
	}

	err := opts.run()
	if err != nil {
		t.Fatalf("failed reloading metadata: %v", err)
	}
}
