package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura actions codegen", func() {

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

	Context("actions codegen tests", func() {
		It("creates the code for all actions specified framework and in directory as in config.yaml file", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"actions", "use-codegen", "--framework", "nodejs-express", "--output-dir", "codegen", "--with-starter-kit", "true"},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"actions", "codegen"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring("Codegen files generated at codegen"))
		})
	})
})
