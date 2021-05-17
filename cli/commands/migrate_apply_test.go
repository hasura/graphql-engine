package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("migrate_apply", func() {

	var dirName string
	var session *Session
	var teardown func()
	BeforeEach(func() {
		dirName = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraVersion)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", dirName},
		})
		editEndpointInConfig(filepath.Join(dirName, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			session.Kill()
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	Context("migrate apply test", func() {
		It("should apply the migrations on server ", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*Applying migrations...*.",
				".*migrations*.",
				".*applied*.",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session.Err, 60*40).Should(Say(keyword))
			}
			Eventually(session, 60*40).Should(Exit(0))
		})
	})

})

var _ = Describe("automatic state migration should not affect new config v3 projects", func() {
	var dirName string
	var session *Session
	var teardown func()
	BeforeEach(func() {
		dirName = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraVersion)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		port, teardownPG := testutil.StartPGContainer(GinkgoT(), "test", "test", "test")
		// add a pg source named default
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, fmt.Sprintf("postgres://test:test@%v:%v/test", testutil.DockerSwitchIP, port), "default")
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", dirName},
		})
		editEndpointInConfig(filepath.Join(dirName, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			session.Kill()
			os.RemoveAll(dirName)
			teardownHGE()
			teardownPG()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("should apply the migrations on server ", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"migrate",
				"create",
				"schema_creation",
				"--up-sql",
				"create schema \"testing\";",
				"--down-sql",
				"drop schema \"testing\" cascade;",
				"--database-name",
				"default",
				"--log-level",
				"debug",
			},
			WorkingDirectory: dirName,
		})
		session = testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"migrate", "apply", "--database-name", "default", "--log-level", "debug"},
			WorkingDirectory: dirName,
		})
		wantKeywordList := []string{
			".*Applying migrations...*.",
			".*migrations*.",
			".*applied*.",
		}

		for _, keyword := range wantKeywordList {
			Eventually(session.Err, 60*40).Should(Say(keyword))
		}
		Eventually(session, 60*40).Should(Exit(0))
	})
})
