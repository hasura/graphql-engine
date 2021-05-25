package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("actions_codegen", func() {

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

	Context("actions codegen tests", func() {
		It("creates the code for all actions spcified framework and in directory as in config.yaml file", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"actions", "use-codegen", "--framework", "nodejs-express", "--output-dir", "codegen", "--with-starter-kit", "true"},
				WorkingDirectory: dirName,
			})
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"actions", "codegen"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*60).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Codegen files generated at codegen"))
		})
	})
})
