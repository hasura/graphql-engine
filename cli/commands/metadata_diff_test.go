package commands

import (
	"bytes"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
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

func TestMetadataDiffCmd(t *testing.T) {
	endpointURL, err := url.Parse(os.Getenv("HASURA_GRAPHQL_TEST_ENDPOINT"))
	if err != nil {
		t.Fatal(err)
	}

	// Create migration Dir
	migrationsDir, err := ioutil.TempDir("", "")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(migrationsDir)

	metadataFile := filepath.Join(migrationsDir, "metadata.yaml")
	testMetadataFile1 := filepath.Join(migrationsDir, "testmetadata1.yaml")
	testMetadataFile2 := filepath.Join(migrationsDir, "testmetadata2.yaml")

	mustWriteFile(t, "", metadataFile, testMetadata1)
	mustWriteFile(t, "", testMetadataFile1, testMetadata1)
	mustWriteFile(t, "", testMetadataFile2, testMetadata2)

	logger, _ := test.NewNullLogger()
	outputFile := new(bytes.Buffer)
	opts := &metadataDiffOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MetadataFile: []string{metadataFile},
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpointURL.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpointURL,
			},
		},
		output: outputFile,
	}

	opts.EC.Version = version.New()
	v, err := version.FetchServerVersion(opts.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	opts.EC.Version.SetServerVersion(v)

	// Run without args
	opts.metadata[0] = metadataFile
	err = opts.run()
	if err != nil {
		t.Fatalf("failed diffing metadata: %v", err)
	}

	// Run with one arg
	opts.metadata = [2]string{testMetadataFile1, ""}

	err = opts.run()
	if err != nil {
		t.Fatalf("failed diffing metadata: %v", err)
	}

	// Run with two args
	opts.metadata = [2]string{testMetadataFile1, testMetadataFile2}

	err = opts.run()
	if err != nil {
		t.Fatalf("failed diffing metadata: %v", err)
	}
}
