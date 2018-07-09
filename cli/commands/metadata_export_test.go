package commands

import (
	"net/url"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus/hooks/test"
)

func testMetadataExport(t *testing.T, executionDir string, endpoint *url.URL) {
	logger, _ := test.NewNullLogger()
	opts := &metadataExportOptions{
		EC: &cli.ExecutionContext{
			Logger:             logger,
			Spinner:            spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			ExecutionDirectory: executionDir,
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:       endpoint.String(),
				AccessKey:      "",
				ParsedEndpoint: endpoint,
			},
		},
		actionType: "export",
	}

	err := opts.run()
	if err != nil {
		t.Fatalf("failed exporting metadata: %v", err)
	}
}
