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

var _ = Describe("hasura init --endpoint (config v3)", func() {
	var projectDirectory string
	var teardown func()
	var hgeEndpoint string
	sourceName := randomdata.SillyName()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, "default", connectionString)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("should create directory with metadata and migrations from server", func() {
		Context("create migrations and apply those on server", func() {
			testMigrateApply(projectDirectory, []string{"--database-name", sourceName})
		})
		Context("test init --endpoint for config v2", func() {
			err := os.RemoveAll(projectDirectory)
			Expect(err).To(BeNil())
			session := testutil.Hasura(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--endpoint", hgeEndpoint, "--fetch"},
			})

			wantKeywordList := []string{
				fmt.Sprintf("cd %s", projectDirectory),
				"hasura console",
				"Metadata exported",
				"migrations applied",
			}
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			fileInfos, err := os.ReadDir(filepath.Join(projectDirectory, "migrations", sourceName))
			Expect(err).To(BeNil())
			Expect(len(fileInfos)).Should(BeEquivalentTo(1))
		})

	})

})

var _ = Describe("hasura init --endpoint (config v2)", func() {
	var projectDirectory string
	var teardown func()
	var hgeEndpoint string

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
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

	It("should create directory with metadata and migrations from server", func() {
		Context("create migrations and apply those on server", func() {
			testMigrateApply(projectDirectory, nil)
		})
		Context("test init --endpoint for config v2", func() {
			err := os.RemoveAll(projectDirectory)
			Expect(err).To(BeNil())
			session := testutil.Hasura(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--endpoint", hgeEndpoint, "--version", "2", "--fetch"},
			})

			wantKeywordList := []string{
				fmt.Sprintf("cd %s", projectDirectory),
				"hasura console",
				"Metadata exported",
				"migrations applied",
			}
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			fileInfos, err := os.ReadDir(filepath.Join(projectDirectory, "migrations"))
			Expect(err).To(BeNil())
			Expect(len(fileInfos)).Should(BeEquivalentTo(1))
		})

	})

})
