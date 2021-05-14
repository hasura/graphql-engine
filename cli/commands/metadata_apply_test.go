package commands

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
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

		Context("metadata dry run ", func() {
			args := strings.Join([]string{
				"hasura", "metadata", "export", "-o", "json",
				"|", "jq", `'.sources[0].configuration.connection_info.pool_settings.max_connections = 51'`,
				"|", "hasura", "md", "apply", "--dry-run",
			}, " ")
			cmd := exec.Command("bash", "-c", args)
			cmd.Dir = dirName
			session, err := Start(
				cmd,
				NewPrefixedWriter(testutil.DebugOutPrefix, GinkgoWriter),
				NewPrefixedWriter(testutil.DebugErrPrefix, GinkgoWriter),
			)
			Expect(err).To(BeNil())
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(string(session.Wait().Out.Contents())).Should(ContainSubstring("\"max_connections\": 51\n"))
		})
		Context("metadata apply metadata from pipe", func() {
			args := strings.Join([]string{
				"hasura", "metadata", "export", "-o", "json",
				"|", "jq", `'.sources[0].configuration.connection_info.pool_settings.max_connections = 51'`,
				"|", "hasura", "md", "apply",
			}, " ")
			cmd := exec.Command("bash", "-c", args)
			cmd.Dir = dirName
			session, err := Start(
				cmd,
				NewPrefixedWriter(testutil.DebugOutPrefix, GinkgoWriter),
				NewPrefixedWriter(testutil.DebugErrPrefix, GinkgoWriter),
			)
			Expect(err).To(BeNil())
			Eventually(session, 60*40).Should(Exit(0))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "export", "--output", "yaml"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			out := session.Wait().Out.Contents()
			Expect(isYAML(out)).To(BeTrue())
			Eventually(string(out)).Should(ContainSubstring("max_connections: 51"))
		})
	})
})
