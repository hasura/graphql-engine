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

		Eventually(session, 60*40).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring(keyword))
		}
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
})

var _ = Describe("hasura migrate apply (config v3)", func() {
	// automatic state migration should not affect new config v3 projects
	var projectDirectory string
	var teardown func()
	var sourceName = randomdata.SillyName()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		// add a pg source named default
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectory},
		})
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
		}
	})
	AfterEach(func() { teardown() })

	It("should apply the migrations on server ", func() {
		testMigrateApply(projectDirectory, []string{"--database-name", sourceName})
	})
})
