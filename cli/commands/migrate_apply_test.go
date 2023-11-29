package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/Pallinder/go-randomdata"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

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
			"migrations applied",
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
			"migrations applied",
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
			"migrations applied",
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

var testMigrateApplyAllDatabases = func(projectDirectory string, databases ...string) {
	Context("migrate apply", func() {
		for _, database := range databases {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", database},
				WorkingDirectory: projectDirectory,
			})
		}

		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"migrate", "apply", "--all-databases"},
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			"migrations applied",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}
	})
}

var testMigrateApplyAllDatabasesWithError = func(projectDirectory string, databases ...string) {
	Context("migrate apply", func() {
		for _, database := range databases {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", database},
				WorkingDirectory: projectDirectory,
			})
		}

		if len(databases) > 0 {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", databases[0]},
				WorkingDirectory: projectDirectory,
			})
		}

		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"migrate", "apply", "--all-databases"},
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			fmt.Sprintf("applying migrations failed on database(s): %s", databases[0]),
		}

		if len(databases) > 0 {
			Eventually(session, timeout).Should(Exit(1))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
		} else {
			Eventually(session, timeout).Should(Exit(0))
		}

	})
}

var testProgressBar = func(projectDirectory string) {
	progressBar := "Applying migrations:  . / 8 .*%"
	Context("migrate apply should display progress bar", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;"},
			WorkingDirectory: projectDirectory,
		})
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"migrate", "apply", "--progressbar-logs"},
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			progressBar,
			"migrations applied",
		}

		Eventually(session, 60*40).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Eventually(session.Err, 60*40).Should(Say(keyword))
		}
	})
	Context("migrate apply shouldn't display progress bar in non-terminal (default behaviour)", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"migrate", "apply", "--down", "all"},
			WorkingDirectory: projectDirectory,
		})

		Eventually(session.Err, 60*40).ShouldNot(Say(progressBar))
		Eventually(session, 60*40).Should(Exit(0))
	})
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
		assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v2/query", createTable)
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
		_, teardownPG := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, pgSource, "postgres")
		_, teardownCitus := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, citusSource, "citus")
		_, teardownMSSQL := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, mssqlSource, "mssql")
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
		assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v2/query", createTable)
	})
	It("should apply the migrations on all-databases", func() {
		testMigrateApplyAllDatabases(projectDirectory, pgSource, citusSource, mssqlSource)
		pgBody := fmt.Sprintf(`
{
    "type": "run_sql",
    "args": {
        "sql": "CREATE TABLE testing.test();",
		"source": "%s"
    }
}
`, pgSource)
		createTable := strings.NewReader(pgBody)
		assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v2/query", createTable)
		citusBody := fmt.Sprintf(`
{
    "type": "citus_run_sql",
    "args": {
        "sql": "CREATE TABLE testing.test();",
		"source": "%s"
    }
}
`, citusSource)
		createTable = strings.NewReader(citusBody)
		assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v2/query", createTable)
	})
	It("should the migrations on all-databases except first database and it should exit with code 1", func() {
		testMigrateApplyAllDatabasesWithError(projectDirectory, pgSource, citusSource, mssqlSource)
		citusBody := fmt.Sprintf(`
{
    "type": "citus_run_sql",
    "args": {
        "sql": "CREATE TABLE testing.test();",
		"source": "%s"
    }
}
`, citusSource)
		createTable := strings.NewReader(citusBody)
		assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v2/query", createTable)
	})
})

var _ = Describe("hasura migrate apply progress bar", func() {
	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
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

	It("test the progress bar of migrate apply", func() {
		testProgressBar(projectDirectory)
	})
})
