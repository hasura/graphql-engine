package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/Pallinder/go-randomdata"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura migrate create (config v3)", func() {
	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectory},
		})
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
		}
	})

	AfterEach(func() { teardown() })

	It("can create migrations for default database", func() {
		migrationName := "create_schema_testing"
		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{
				"migrate",
				"create",
				migrationName,
				"--up-sql",
				"create schema \"testing\";",
				"--down-sql",
				"drop schema \"testing\" cascade;",
				"--database-name", "default",
			},
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			"Migrations files created",
			migrationName,
			"version",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}
		dirs, err := os.ReadDir(filepath.Join(projectDirectory, "migrations", "default"))
		Expect(err).To(BeNil())
		for _, d := range dirs {
			Expect(d.Name()).Should(ContainSubstring(migrationName))
		}
	})

	It("can create empty migrations when no data is provided", func() {
		migrationName := "no_data_migration"
		sourceName := randomdata.SillyName()
		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{
				"migrate",
				"create",
				migrationName,
				"--database-name", sourceName,
			},
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			"Migrations files created",
			migrationName,
			"version",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}
		dirs, err := os.ReadDir(filepath.Join(projectDirectory, "migrations", sourceName))
		Expect(err).To(BeNil())
		for _, d := range dirs {
			Expect(d.Name()).Should(ContainSubstring(migrationName))
		}
	})

	It("can create migrations for database that is not connected to server", func() {
		migrationName := "create_schema_testing"
		sourceName := randomdata.SillyName()
		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{
				"migrate",
				"create",
				migrationName,
				"--up-sql",
				"create schema \"testing\";",
				"--down-sql",
				"drop schema \"testing\" cascade;",
				"--database-name", sourceName,
			},
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			fmt.Sprintf("database '%s' is not connected to hasura", sourceName),
			"Migrations files created",
			migrationName,
			"version",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}
		dirs, err := os.ReadDir(filepath.Join(projectDirectory, "migrations", sourceName))
		Expect(err).To(BeNil())
		for _, d := range dirs {
			Expect(d.Name()).Should(ContainSubstring(migrationName))
		}
	})
})

var _ = Describe("hasura migrate create (config v2)", func() {
	var projectDirectoryLatest, projectDirectoryV13 string
	var session *Session
	var teardown func()
	BeforeEach(func() {
		projectDirectoryLatest = testutil.RandDirName()
		hgeEndPortLatest, teardownHGELatest := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpointLatest := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortLatest)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryLatest, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryLatest, defaultConfigFilename), hgeEndpointLatest)

		projectDirectoryV13 = testutil.RandDirName()
		hgeEndPortV13, teardownHGEV13 := testutil.StartHasura(GinkgoT(), "hasura/graphql-engine:v1.3.3")
		hgeEndpointV13 := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortV13)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryV13, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryV13, defaultConfigFilename), hgeEndpointV13)

		teardown = func() {
			session.Kill()
			os.RemoveAll(projectDirectoryLatest)
			os.RemoveAll(projectDirectoryV13)
			teardownHGEV13()
			teardownHGELatest()
		}
	})

	AfterEach(func() { teardown() })

	It("can create migration files", func() {
		test := func(projectDirectory string) {
			migrationName := "create_schema_testing"
			wantKeywordList := []string{
				"Migrations files created",
				migrationName,
				"version",
			}
			session = testutil.Hasura(testutil.CmdOpts{
				Args: []string{
					"migrate",
					"create",
					migrationName,
					"--up-sql", "create schema \"testing\";",
					"--down-sql",
					"drop schema \"testing\" cascade;",
				},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}

			dirs, err := os.ReadDir(filepath.Join(projectDirectory, "migrations"))
			Expect(err).To(BeNil())
			for _, d := range dirs {
				Expect(d.Name()).Should(ContainSubstring(migrationName))
			}
		}
		test(projectDirectoryLatest)
		test(projectDirectoryV13)
	})
})
