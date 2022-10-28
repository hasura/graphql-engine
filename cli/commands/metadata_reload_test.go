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

var _ = Describe("hasura metadata reload", func() {

	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		copyTestConfigV2Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
		}
	})

	AfterEach(func() { teardown() })

	Context("metadata reload test", func() {
		It("should reload metadata", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "reload"},
				WorkingDirectory: projectDirectory,
			})
			want := `Metadata reloaded`
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring(want))
		})
	})

	Context("metadata reload test incase of inconsistent metadata", func() {
		It("should reload the metadata on server and get inconsistent metadata", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--up", "all"},
				WorkingDirectory: projectDirectory,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply"},
				WorkingDirectory: projectDirectory,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--down", "all"},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "status"},
				WorkingDirectory: projectDirectory,
			})
			want := `metadata is consistent`
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring(want))
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "reload"},
				WorkingDirectory: projectDirectory,
			})
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "status"},
				WorkingDirectory: projectDirectory,
			})
			want = `metadata is inconsistent, use 'hasura metadata ic list' command to see the inconsistent objects`
			Eventually(session.Wait(timeout)).Should(Exit(1))
			Expect(session.Err.Contents()).Should(ContainSubstring(want))
		})
	})
})
