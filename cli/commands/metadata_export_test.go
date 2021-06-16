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

var testExportMetadataToStdout = func(projectDirectory string) {
	Context("metadata export to stdout", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "export", "-o", "yaml"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, 60*40).Should(Exit(0))
		stdout := session.Wait().Out.Contents()
		Eventually(isYAML(stdout)).Should(BeTrue())

		session = testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "export", "--output", "json"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, 60*40).Should(Exit(0))
		stdout = session.Wait().Out.Contents()
		Eventually(isJSON(stdout)).Should(BeTrue())
	})
}

var _ = Describe("hasura metadata export (config v3)", func() {
	var projectDirectory string
	var teardown func()
	sourceName := randomdata.SillyName()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)

		// clone template project directory as test project directory
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, sourceName, connectionString)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		// remove the directory after apply to check if it gets recreated after a successful export
		Expect(os.RemoveAll(filepath.Join(projectDirectory, "metadata", "databases", sourceName))).To(BeNil())

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
		}
	})

	AfterEach(func() { teardown() })

	It("can export metadata from server", func() {
		Context("metadata export to project directory", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "export"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Metadata exported"))
			Expect(filepath.Join(projectDirectory, "metadata", "databases", sourceName, "tables", "public_albums.yaml")).Should(BeAnExistingFile())
		})
		testExportMetadataToStdout(projectDirectory)
	})

})

var _ = Describe("hasura metadata export (config v2)", func() {
	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		// clone template project directory as test project directory
		copyTestConfigV2Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"migrate", "apply", "&&", testutil.CLIBinaryPath, "metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		// remove the directory after apply to check if it gets recreated after a successful export
		Expect(os.RemoveAll(filepath.Join(projectDirectory, "metadata"))).To(BeNil())

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
		}
	})

	AfterEach(func() { teardown() })

	It("can export metadata from server", func() {
		Context("metadata export to project directory", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "export"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Metadata exported"))
			Expect(filepath.Join(projectDirectory, "metadata", "tables.yaml")).Should(BeAnExistingFile())
		})
		testExportMetadataToStdout(projectDirectory)
	})

})
