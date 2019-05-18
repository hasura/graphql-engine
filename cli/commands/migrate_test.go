package commands

import (
	"database/sql"
	sqldriver "database/sql/driver"
	"fmt"
	"io"
	"io/ioutil"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"testing"

	"github.com/Masterminds/semver"

	"github.com/hasura/graphql-engine/cli/migrate"
	mt "github.com/hasura/graphql-engine/cli/migrate/testing"
	"github.com/hasura/graphql-engine/cli/version"
	_ "github.com/lib/pq"
	"github.com/parnurzeal/gorequest"
	"github.com/stretchr/testify/assert"
)

var postgresVersions = []mt.Version{
	{Image: "postgres:9.6"},
}

var ravenVersions = []mt.Version{
	{Image: "hasura/graphql-engine:190d78e", Cmd: []string{"raven", "serve", "--database-url"}, ExposedPort: 8080},
}

var testMetadataPrev = map[string][]byte{
	"metadata": []byte(`functions: []
query_templates: []
remote_schemas: []
tables:
- array_relationships: []
  delete_permissions: []
  event_triggers: []
  insert_permissions: []
  object_relationships: []
  select_permissions: []
  table: test
  update_permissions: []
`),
	"empty-metadata": []byte(`functions: []
query_templates: []
remote_schemas: []
tables: []
`),
}

var testMetadataCurrent = map[string][]byte{
	"metadata": []byte(`allowlist: []
functions: []
query_collections: []
query_templates: []
remote_schemas: []
tables:
- array_relationships: []
  delete_permissions: []
  event_triggers: []
  insert_permissions: []
  object_relationships: []
  select_permissions: []
  table: test
  update_permissions: []
`),
	"empty-metadata": []byte(`allowlist: []
functions: []
query_collections: []
query_templates: []
remote_schemas: []
tables: []
`),
}

func isReadyPostgres(i mt.Instance) bool {
	db, err := sql.Open("postgres", fmt.Sprintf("postgres://postgres@%v:%v/postgres?sslmode=disable", i.Host(), i.Port()))
	if err != nil {
		return false
	}

	defer db.Close()
	if err = db.Ping(); err != nil {
		switch err {
		case sqldriver.ErrBadConn, io.EOF:
			return false
		default:
			fmt.Println(err)
		}
		return false
	}
	return true
}

func isReadyRaven(i mt.Instance) bool {
	request := gorequest.New()
	_, _, errs := request.Post(fmt.Sprintf("http://%s:%d", i.Host(), i.Port())).End()
	if len(errs) == 0 {
		return true
	}
	return false
}

func testMigrateWithDocker(t *testing.T, migrationsDir, executionDir string) {
	mt.ParallelTest(t, postgresVersions, isReadyPostgres,
		func(t *testing.T, pi mt.Instance) {
			for i, v := range ravenVersions {
				ravenVersions[i].Cmd = append(v.Cmd, fmt.Sprintf("postgres://postgres@%v:%v/postgres?sslmode=disable", pi.NetworkSettings().Gateway, pi.Port()))
			}
			mt.ParallelTest(t, ravenVersions, isReadyRaven,
				func(t *testing.T, ri mt.Instance) {
					defer pi.Remove()
					defer ri.Remove()

					endpointURL, err := url.Parse(fmt.Sprintf("http://%s:%d", ri.Host(), ri.Port()))
					if err != nil {
						t.Fatal(err)
					}
					// Create migration Dir
					migrationsDir, err := ioutil.TempDir("", "")
					if err != nil {
						t.Fatal(err)
					}
					defer os.RemoveAll(migrationsDir)

					testMigrate(t, endpointURL, migrationsDir)
				})
		})
}

func TestMigrateCmd(t *testing.T) {
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

	testMigrate(t, endpointURL, migrationsDir)
}

func testMigrate(t *testing.T, endpoint *url.URL, migrationsDir string) {
	versionCtx := version.New()
	v, err := version.FetchServerVersion(endpoint.String())
	if err != nil {
		t.Fatal(err)
	}
	versionCtx.SetServerVersion(v)

	metadataFile := filepath.Join(migrationsDir, "metadata.yaml")
	// Create 1_create_table_test.up.sql which creates table test
	mustWriteFile(t, migrationsDir, "1_create_table_test.up.sql", `CREATE TABLE "test"("id" serial NOT NULL, PRIMARY KEY ("id") )`)
	// Create 1_create_table_test.down.sql which creates table test
	mustWriteFile(t, migrationsDir, "1_create_table_test.down.sql", `DROP TABLE "test";`)
	// Create 2_add_table_test.up.yaml which adds table test to metadata
	mustWriteFile(t, migrationsDir, "2_add_table_test.up.yaml", `- args:
    name: test
  type: add_existing_table_or_view
`)
	mustWriteFile(t, migrationsDir, "2_add_table_test.down.yaml", `- args:
    table: test
  type: untrack_table
`)
	mustWriteFile(t, migrationsDir, "2_add_table_test.up.sql", `CREATE TABLE "author"("id" serial NOT NULL, PRIMARY KEY ("id") )`)
	mustWriteFile(t, migrationsDir, "2_add_table_test.down.sql", `DROP TABLE "author";`)

	// Apply 1_create_table_test.up.sql
	testMigrateApply(t, endpoint, migrationsDir, "1", "", "", "")

	// Check Migration status
	expectedStatus := migrate.NewStatus()
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   1,
		IsApplied: true,
		IsPresent: true,
	})
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   2,
		IsApplied: false,
		IsPresent: true,
	})
	testMigrateStatus(t, endpoint, migrationsDir, expectedStatus)

	// Apply 2_add_table_test.up.yaml
	testMigrateApply(t, endpoint, migrationsDir, "", "", "2", "")

	// Check Migration status
	expectedStatus = migrate.NewStatus()
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   1,
		IsApplied: true,
		IsPresent: true,
	})
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   2,
		IsApplied: true,
		IsPresent: true,
	})
	testMigrateStatus(t, endpoint, migrationsDir, expectedStatus)

	// Apply 2_add_table_test.down.yaml
	testMigrateApply(t, endpoint, migrationsDir, "", "1", "", "")

	// Check Migration status
	expectedStatus = migrate.NewStatus()
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   1,
		IsApplied: true,
		IsPresent: true,
	})
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   2,
		IsApplied: false,
		IsPresent: true,
	})
	testMigrateStatus(t, endpoint, migrationsDir, expectedStatus)

	// Apply 1_create_table_test.down.sql
	testMigrateApply(t, endpoint, migrationsDir, "", "", "1", "down")

	// Check Migration status
	expectedStatus = migrate.NewStatus()
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   1,
		IsApplied: false,
		IsPresent: true,
	})
	expectedStatus.Append(&migrate.MigrationStatus{
		Version:   2,
		IsApplied: false,
		IsPresent: true,
	})
	testMigrateStatus(t, endpoint, migrationsDir, expectedStatus)

	// Apply both 1 and 2
	testMigrateApply(t, endpoint, migrationsDir, "", "", "", "")

	testMetadataExport(t, metadataFile, endpoint)
	compareMetadata(t, metadataFile, "metadata", versionCtx.ServerSemver)

	testMetadataApply(t, metadataFile, endpoint)
	testMetadataExport(t, metadataFile, endpoint)
	compareMetadata(t, metadataFile, "metadata", versionCtx.ServerSemver)

	testMetadataReset(t, metadataFile, endpoint)
	testMetadataExport(t, metadataFile, endpoint)
	compareMetadata(t, metadataFile, "empty-metadata", versionCtx.ServerSemver)
}

func mustWriteFile(t testing.TB, dir, file string, body string) {
	if err := ioutil.WriteFile(path.Join(dir, file), []byte(body), 06444); err != nil {
		t.Fatal(err)
	}
}

func compareMetadata(t testing.TB, metadataFile string, actualType string, serverVersion *semver.Version) {
	var actualData []byte
	c, err := semver.NewConstraint("<= 1.0.0-alpha45")
	if err != nil {
		t.Fatal(err)
	}
	if serverVersion == nil || !c.Check(serverVersion) {
		actualData = testMetadataCurrent[actualType]
	} else {
		actualData = testMetadataPrev[actualType]
	}
	data, err := ioutil.ReadFile(metadataFile)
	if err != nil {
		t.Fatalf("error reading metadata %s", err)
	}
	assert.Equal(t, string(actualData), string(data))
}
