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

var _ = Describe("hasura metadata clear", func() {

	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		t := GinkgoT()

		sourceName := randomdata.SillyName()
		connectionString, teardownPG := testutil.StartPGContainer(t)

		hgeEndPort, teardownHGE := testutil.StartHasuraWithPG(t, testutil.HasuraDockerImage, connectionString)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

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

	Context("metadata clear test", func() {
		It("should clear metadata on server", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply"},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "clear"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring("Metadata cleared"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "diff"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			stdout := session.Out.Contents()
			Expect(stdout).Should(ContainSubstring("tables"))

		})
	})
})
