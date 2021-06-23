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

var commonMetadataCommandsTest = func(projectDirectory string) {
	Context("should apply metadata to server", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, 60*40).Should(Exit(0))
		Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Metadata applied"))
	})
	Context("apply metadata to server and it should output the metadata of project to stdout", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply", "--output", "json"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, 60*40).Should(Exit(0))
		Eventually(isJSON(session.Wait().Out.Contents())).Should(BeTrue())
	})
}

var testConfigV2 = func(projectDirectory string) {
	Context("apply migrations", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"migrate", "apply"},
			WorkingDirectory: projectDirectory,
		})
	})
	commonMetadataCommandsTest(projectDirectory)
}

var _ = Describe("hasura metadata apply (config v3)", func() {
	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		sourceName := randomdata.SillyName()
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
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

	It("metadata apply", func() {
		commonMetadataCommandsTest(projectDirectory)
	})
})

var _ = Describe("hasura metadata apply (config v2)", func() {
	var projectDirectoryLatest, projectDirectoryV13 string
	var teardown func()
	BeforeEach(func() {
		projectDirectoryLatest = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		copyTestConfigV2Project(projectDirectoryLatest)
		editEndpointInConfig(filepath.Join(projectDirectoryLatest, defaultConfigFilename), hgeEndpoint)

		projectDirectoryV13 = testutil.RandDirName()
		hgeEndPortV13, teardownHGEV13 := testutil.StartHasura(GinkgoT(), "hasura/graphql-engine:v1.3.3")
		hgeEndpointV13 := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortV13)
		copyTestConfigV2Project(projectDirectoryV13)
		editEndpointInConfig(filepath.Join(projectDirectoryV13, defaultConfigFilename), hgeEndpointV13)

		teardown = func() {
			os.RemoveAll(projectDirectoryLatest)
			os.RemoveAll(projectDirectoryV13)
			teardownHGE()
			teardownHGEV13()
		}
	})

	AfterEach(func() { teardown() })

	It("metadata apply", func() {
		testConfigV2(projectDirectoryLatest)
		testConfigV2(projectDirectoryV13)
	})
})
