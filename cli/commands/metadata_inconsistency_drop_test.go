package commands

import (
	"net/url"
	"os"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/sirupsen/logrus/hooks/test"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/version"
)

func testMetadataInconsistencyDropCmd(t *testing.T, migrationsDir string, metadataFile string, endpoint *url.URL) {
	logger, _ := test.NewNullLogger()
	opts := &metadataInconsistencyDropOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: []string{metadataFile},
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpoint.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpoint,
			},
			MigrationDir: migrationsDir,
		},
	}

	opts.EC.Version = version.New()
	v, err := version.FetchServerVersion(opts.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	opts.EC.Version.SetServerVersion(v)

	err = opts.run()
	if err != nil {
		t.Fatalf("failed dropping the inconsistency: %v", err)
	}

	os.RemoveAll(opts.EC.MigrationDir)
}
