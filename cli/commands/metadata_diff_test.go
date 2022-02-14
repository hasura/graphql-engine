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

	AfterEach(func() {
		teardown()
	})

	Context("metadata diff test", func() {
		It("should output diff between metadata on server and local project", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "diff"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			stdout := session.Out.Contents()
			Expect(stdout).Should(ContainSubstring("kind: postgres"))
			Expect(stdout).Should(ContainSubstring("name: default"))

			editMetadataFileInConfig(filepath.Join(projectDirectory, defaultConfigFilename), "metadata.yaml")
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "diff"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			stdout = session.Out.Contents()
			Expect(stdout).Should(ContainSubstring("sources"))
			Expect(stdout).Should(ContainSubstring("kind: postgres"))
			Expect(stdout).Should(ContainSubstring("name: default"))
			Expect(stdout).Should(ContainSubstring("tables: []"))

			editMetadataFileInConfig(filepath.Join(projectDirectory, defaultConfigFilename), "metadata.json")
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "diff", "--no-color"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			stdout = session.Out.Contents()
			want := `-  "version": 3,
-  "sources": [
-    {
-      "name": "default",
-      "kind": "postgres",
-      "tables": [],
-      "configuration": {
`

			Expect(stdout).Should(ContainSubstring(want))
		})
	})
})
