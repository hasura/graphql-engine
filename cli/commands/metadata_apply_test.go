package commands

import (
	"fmt"
	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
	"os"
	"path/filepath"
)

var _ = Describe("metadata_apply", func() {
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

	It("metadata apply", func() {
		Context("should apply metadata to server", func() {
			session = testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "export"},
				WorkingDirectory: dirName,
			})
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "apply"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Metadata applied"))
		})
		Context("apply metadata to server and it should output the metadata of project to stdout", func() {
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "apply", "--output", "json"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(isJSON(session.Wait().Out.Contents())).Should(BeTrue())
		})
	})
})
