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

var _ = Describe("hasura metadata diff", func() {

	var dirName string
	var teardown func()
	BeforeEach(func() {
		dirName = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", dirName},
		})
		editEndpointInConfig(filepath.Join(dirName, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	Context("metadata diff test", func() {
		It("should output diff between metadata on server and local project", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "diff"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(session.Wait().Out.Contents()).Should(ContainSubstring("-sources: []"))
			Eventually(session.Wait().Out.Contents()).Should(ContainSubstring("+sources:"))
			Eventually(session.Wait().Out.Contents()).Should(ContainSubstring("+  kind: postgres"))
			Eventually(session.Wait().Out.Contents()).Should(ContainSubstring("+  name: default"))
			Eventually(session.Wait().Out.Contents()).Should(ContainSubstring("+  tables: []"))
		})
	})
})
