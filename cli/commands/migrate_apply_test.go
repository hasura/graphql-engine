package commands

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/Pallinder/go-randomdata"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

func AddDatabaseToHasura(hgeEndpoint, sourceName, databaseKind string) (string, func()) {

	if databaseKind == "postgres" {
		connectionStringPG, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionStringPG, sourceName)
		return connectionStringPG, teardownPG
	}
	if databaseKind == "citus" {
		connectionStringCitus, teardownCitus := testutil.StartCitusContainer(GinkgoT())
		testutil.AddCitusSourceToHasura(GinkgoT(), hgeEndpoint, connectionStringCitus, sourceName)
		return connectionStringCitus, teardownCitus
	}

	if databaseKind == "mssql" {
		mssqlPort, teardownMSSQL := testutil.StartMSSQLContainer(GinkgoT())
		connectionStringMSSQL := fmt.Sprintf("DRIVER={ODBC Driver 17 for SQL Server};SERVER=%s,%s;DATABASE=master;Uid=SA;Pwd=%s;Encrypt=no", testutil.DockerSwitchIP, mssqlPort, testutil.MSSQLPassword)
		testutil.AddMSSQLSourceToHasura(GinkgoT(), hgeEndpoint, connectionStringMSSQL, sourceName)
		return connectionStringMSSQL, teardownMSSQL

	}

	return "", nil
}

var testMigrateApply = func(projectDirectory string, globalFlags []string) {
	Context("migrate apply", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: append(
				[]string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;"},
				globalFlags...,
			),
			WorkingDirectory: projectDirectory,
		})
		session := testutil.Hasura(testutil.CmdOpts{
			Args: append(
				[]string{"migrate", "apply"},
				globalFlags...,
			),
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			"Applying migrations",
			"applied",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}
	})
}

var testMigrateApplySkipExecution = func(projectDirectory string, globalFlags []string) {
	Context("migrate apply --skip-execution", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: append(
				[]string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;"},
				globalFlags...,
			),
			WorkingDirectory: projectDirectory,
		})
		session := testutil.Hasura(testutil.CmdOpts{
			Args: append(
				[]string{"migrate", "apply"},
				globalFlags...,
			),
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			"Applying migrations",
			"applied",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		session = testutil.Hasura(testutil.CmdOpts{
			Args: append(
				[]string{"migrate", "apply", "--skip-execution", "--down", "all"},
				globalFlags...,
			),
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList = []string{
			"Applying migrations",
			"applied",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		session = testutil.Hasura(testutil.CmdOpts{
			Args: append(
				[]string{"migrate", "status"},
				globalFlags...,
			),
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList = []string{
			".*VERSION*.",
			".*SOURCE STATUS*.",
			".*DATABASE STATUS*.",
			".*schema_creation*.",
			".*Present        Not Present*.",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Wait(timeout).Out).Should(Say(keyword))
		}

	})
}

var testByRunningAPI = func(hgeEndpoint string, urlPath string, body io.Reader) {
	uri, err := url.Parse(hgeEndpoint)
	Expect(err).To(BeNil())
	uri.Path = path.Join(uri.Path, urlPath)
	req, err := http.NewRequest("POST", uri.String(), body)
	Expect(err).To(BeNil())

	req.Header.Set("Content-Type", "application/json")
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if adminSecret != "" {
		req.Header.Set("x-hasura-admin-secret", adminSecret)
	}

	resp, err := http.DefaultClient.Do(req)
	Expect(err).To(BeNil())
	defer resp.Body.Close()
	Expect(fmt.Sprint(resp.StatusCode)).Should(ContainSubstring(fmt.Sprint(http.StatusOK)))

}

var _ = Describe("hasura migrate apply", func() {
	var hgeEndpoint string
	var teardown func()
	BeforeEach(func() {
		var hgeEndPort string
		hgeEndPort, teardown = testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
	})

	AfterEach(func() { teardown() })

	It("can apply the migrations on server (single database config v3)", func() {
		projectDirectoryV3 := testutil.RandDirName()
		defer os.RemoveAll(projectDirectoryV3)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryV3},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryV3, defaultConfigFilename), hgeEndpoint)
		testMigrateApply(projectDirectoryV3, []string{"--database-name", "default"})
	})
	It("can apply the migrations on server (single database config v2)", func() {
		projectDirectoryV2 := testutil.RandDirName()
		defer os.RemoveAll(projectDirectoryV2)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryV2, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryV2, defaultConfigFilename), hgeEndpoint)
		testMigrateApply(projectDirectoryV2, nil)
	})
	It("should mark the migrations as apply ", func() {
		projectDirectoryV2 := testutil.RandDirName()
		defer os.RemoveAll(projectDirectoryV2)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryV2, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryV2, defaultConfigFilename), hgeEndpoint)
		testMigrateApplySkipExecution(projectDirectoryV2, nil)
		createTable := strings.NewReader(`
		{
			"type": "run_sql",
			"args": {
				"sql": "CREATE TABLE testing.test();"
			}
		}
		`)
		testByRunningAPI(hgeEndpoint, "v2/query", createTable)
	})
})

var _ = Describe("hasura migrate apply (config v3)", func() {
	// automatic state migration should not affect new config v3 projects
	var projectDirectory string
	var hgeEndpoint string
	var teardown func()
	var pgSource = randomdata.SillyName()
	var citusSource = randomdata.SillyName()
	var mssqlSource = randomdata.SillyName()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		_, teardownPG := AddDatabaseToHasura(hgeEndpoint, pgSource, "postgres")
		_, teardownCitus := AddDatabaseToHasura(hgeEndpoint, citusSource, "citus")
		_, teardownMSSQL := AddDatabaseToHasura(hgeEndpoint, mssqlSource, "mssql")
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectory},
		})
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
			teardownCitus()
			teardownMSSQL()
		}
	})
	AfterEach(func() { teardown() })

	It("should apply the migrations on server", func() {
		testMigrateApply(projectDirectory, []string{"--database-name", pgSource})
		testMigrateApply(projectDirectory, []string{"--database-name", citusSource})
		testMigrateApply(projectDirectory, []string{"--database-name", mssqlSource})
	})
	It("should mark the migrations as applied ", func() {
		testMigrateApplySkipExecution(projectDirectory, []string{"--database-name", pgSource})

		createTableString := fmt.Sprintf(`
{
    "type": "run_sql",
    "args": {
        "sql": "CREATE TABLE testing.test();",
		"source": "%s"
    }
}
`, pgSource)
		createTable := strings.NewReader(createTableString)
		testByRunningAPI(hgeEndpoint, "v2/query", createTable)
	})
})
