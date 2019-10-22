package commands

import (
	"bytes"
	"net/url"
	"os"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/sirupsen/logrus/hooks/test"
)

var testMetadata1 = `allowlist: []
functions: []
query_collections: []
remote_schemas: []
tables:
- array_relationships: []
  delete_permissions: []
  event_triggers: []
  insert_permissions: []
  is_enum: false
  object_relationships: []
  select_permissions: []
  table: test
  update_permissions: []
`

var testMetadata2 = `allowlist: []
functions: []
query_collections: []
remote_schemas: []
tables:
- array_relationships: []
  configuration:
    custom_column_names: {}
    custom_root_fields:
      delete: null
      insert: null
      select: null
      select_aggregate: null
      select_by_pk: null
      update: null
  delete_permissions: []
  event_triggers: []
  insert_permissions: []
  is_enum: false
  object_relationships: []
  select_permissions: []
  table: test
  update_permissions: []
`

func testMetadataDiff(t *testing.T, metadataFile string, endpoint *url.URL) {
	logger, _ := test.NewNullLogger()
	outputFile := new(bytes.Buffer)
	optsNoArgs := &metadataDiffOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: []string{metadataFile},
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpoint.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpoint,
			},
		},
		output: outputFile,
	}

	optsNoArgs.EC.Version = version.New()
	v, err := version.FetchServerVersion(optsNoArgs.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	optsNoArgs.EC.Version.SetServerVersion(v)

	// Run without args
	err = optsNoArgs.run()
	if err != nil {
		t.Fatalf("failed diffing metadata: %v", err)
	}

	optsOneArg := &metadataDiffOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: []string{metadataFile},
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpoint.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpoint,
			},
		},
		output: outputFile,
		metaDataFiles: [2]string{"testmetadata1.yaml", ""},
	}
	mustWriteFile(t, "", "testmetadata1.yaml", testMetadata1)

	optsOneArg.EC.Version = version.New()
	v, err = version.FetchServerVersion(optsOneArg.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	optsOneArg.EC.Version.SetServerVersion(v)

	// Run with one arg
	err = optsOneArg.run()
	if err != nil {
		t.Fatalf("failed diffing metadata: %v", err)
	}

	optsTwoArgs := &metadataDiffOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: []string{metadataFile},
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpoint.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpoint,
			},
		},
		output: outputFile,
		metaDataFiles: [2]string{"testmetadata1.yaml", "testmetadata2.yaml"},
	}
	mustWriteFile(t, "", "testmetadata2.yaml", testMetadata2)

	optsOneArg.EC.Version = version.New()
	v, err = version.FetchServerVersion(optsOneArg.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	optsOneArg.EC.Version.SetServerVersion(v)

	// Run with two args
	err = optsTwoArgs.run()
	if err != nil {
		t.Fatalf("failed diffing metadata: %v", err)
	}
}
