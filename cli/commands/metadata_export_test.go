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

var _ = Describe("metadata_export", func() {

	var dirName string
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
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	Context("metadata export test", func() {
		It("should export metadata from server", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "export"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Metadata exported"))
		})
	})

	Context("metadata export with output formats", func() {
		It("should export metadata from server to stdout", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "export", "-o", "yaml"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			stdout := session.Wait().Out.Contents()
			Eventually(isYAML(stdout)).Should(BeTrue())

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "export", "--output", "json"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			stdout = session.Wait().Out.Contents()
			Eventually(isJSON(stdout)).Should(BeTrue())
		})
	})
})
