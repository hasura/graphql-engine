package commands

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/Pallinder/go-randomdata"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

func TestMetadataApplySourcesRequestType(t *testing.T) {
	t.Parallel()

	testCases := []struct {
		name      string
		kind      string
		operation string
		want      string
		wantErr   bool
	}{
		{name: "postgres add", kind: "postgres", operation: "add", want: "pg_add_source"},
		{name: "postgres update", kind: "postgres", operation: "update", want: "pg_update_source"},
		{name: "mssql add", kind: "mssql", operation: "add", want: "mssql_add_source"},
		{name: "citus add", kind: "citus", operation: "add", want: "citus_add_source"},
		{name: "cockroach add", kind: "cockroach", operation: "add", want: "cockroach_add_source"},
		{name: "bigquery add", kind: "bigquery", operation: "add", want: "bigquery_add_source"},
		{name: "unsupported backend", kind: "sqlite", operation: "add", wantErr: true},
	}

	for _, tt := range testCases {
		t.Run(tt.name, func(t *testing.T) {
			got, err := metadataApplySourcesRequestType(tt.kind, tt.operation)
			if tt.wantErr {
				if err == nil {
					t.Fatalf("expected error, got nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got != tt.want {
				t.Fatalf("got %q, want %q", got, tt.want)
			}
		})
	}
}

func TestLoadMetadataApplySources(t *testing.T) {
	t.Parallel()

	sources, err := loadMetadataApplySources(filepath.Join("testdata", "config-v3-test-project", "metadata", "databases"))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(sources) != 1 {
		t.Fatalf("got %d sources, want 1", len(sources))
	}
	if sources[0].Name != "pg" {
		t.Fatalf("got source name %q, want %q", sources[0].Name, "pg")
	}
	if sources[0].Kind != "postgres" {
		t.Fatalf("got source kind %q, want %q", sources[0].Kind, "postgres")
	}
	configuration, ok := sources[0].Configuration.(map[string]interface{})
	if !ok {
		t.Fatalf("configuration has unexpected type %T", sources[0].Configuration)
	}
	connectionInfo, ok := configuration["connection_info"].(map[string]interface{})
	if !ok {
		t.Fatalf("connection_info has unexpected type %T", configuration["connection_info"])
	}
	if connectionInfo["database_url"] != "TO_BE_FILLED" {
		t.Fatalf("got database_url %v, want %q", connectionInfo["database_url"], "TO_BE_FILLED")
	}
	sourceJSON, err := json.Marshal(sources[0])
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if strings.Contains(string(sourceJSON), `"tables"`) {
		t.Fatalf("unexpected tables key in source payload: %s", string(sourceJSON))
	}
	if strings.Contains(string(sourceJSON), `"functions"`) {
		t.Fatalf("unexpected functions key in source payload: %s", string(sourceJSON))
	}
}

type metadataApplySourcesExportedMetadata struct {
	Sources []metadataApplySourcesExportedSource `json:"sources"`
}

type metadataApplySourcesExportedSource struct {
	Name          string            `json:"name"`
	Kind          string            `json:"kind"`
	Tables        []json.RawMessage `json:"tables"`
	Configuration struct {
		ConnectionInfo struct {
			DatabaseURL           string `json:"database_url"`
			UsePreparedStatements bool   `json:"use_prepared_statements"`
			PoolSettings          struct {
				ConnectionLifetime int `json:"connection_lifetime"`
			} `json:"pool_settings"`
		} `json:"connection_info"`
	} `json:"configuration"`
}

func exportMetadataFromServer(hgeEndpoint string) metadataApplySourcesExportedMetadata {
	response := assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v1/metadata", strings.NewReader(`{"type":"export_metadata","args":{}}`))
	var metadata metadataApplySourcesExportedMetadata
	Expect(json.Unmarshal(response, &metadata)).To(BeNil())
	return metadata
}

func findExportedSource(metadata metadataApplySourcesExportedMetadata, sourceName string) *metadataApplySourcesExportedSource {
	for idx := range metadata.Sources {
		if metadata.Sources[idx].Name == sourceName {
			return &metadata.Sources[idx]
		}
	}
	return nil
}

var _ = Describe("hasura metadata apply-sources (config v3)", func() {
	var projectDirectory, hgeEndpoint, connectionString, sourceName string
	var teardown func()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgePort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgePort)

		sourceName = randomdata.SillyName()
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, sourceName, connectionString)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownPG()
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("creates missing sources without applying tracked tables", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply-sources"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		Expect(session.Err.Contents()).Should(ContainSubstring("Source configurations applied"))

		metadata := exportMetadataFromServer(hgeEndpoint)
		source := findExportedSource(metadata, sourceName)
		Expect(source).ShouldNot(BeNil())
		Expect(source.Kind).To(Equal("postgres"))
		Expect(source.Tables).To(HaveLen(0))
		Expect(source.Configuration.ConnectionInfo.DatabaseURL).To(Equal(connectionString))
		Expect(source.Configuration.ConnectionInfo.UsePreparedStatements).To(BeFalse())
		Expect(source.Configuration.ConnectionInfo.PoolSettings.ConnectionLifetime).To(Equal(600))
	})

	It("updates existing sources with replace_configuration semantics", func() {
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)

		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply-sources"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		Expect(session.Err.Contents()).Should(ContainSubstring("Source configurations applied"))

		metadata := exportMetadataFromServer(hgeEndpoint)
		source := findExportedSource(metadata, sourceName)
		Expect(source).ShouldNot(BeNil())
		Expect(source.Tables).To(HaveLen(0))
		Expect(source.Configuration.ConnectionInfo.DatabaseURL).To(Equal(connectionString))
		Expect(source.Configuration.ConnectionInfo.UsePreparedStatements).To(BeFalse())
		Expect(source.Configuration.ConnectionInfo.PoolSettings.ConnectionLifetime).To(Equal(600))
	})
})

var _ = Describe("hasura metadata apply-sources (config v2)", func() {
	var projectDirectory, hgeEndpoint string
	var teardown func()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgePort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgePort)
		copyTestConfigV2Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("fails clearly", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply-sources"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(1))
		Expect(session.Err.Contents()).Should(ContainSubstring("metadata apply-sources is only supported for config v3 projects"))
	})
})
