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

var _ = Describe("hasura metadata inconsistency drop", func() {

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

	AfterEach(func() { teardown() })

	Context("metadata inconsistency drop test", func() {
		It("Drops inconsistent objects from the metadata", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "drop"},
				WorkingDirectory: dirName,
			})
			want := `all inconsistent objects removed from metadata`
			Eventually(session, 60*40).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring(want))
		})
	})
})
