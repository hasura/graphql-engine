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

var _ = Describe("hasura metadata inconsistency status", func() {

	var projectDirectory string
	var sourceName string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		sourceName = randomdata.SillyName()
		connectionString, teardownPG := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, sourceName, "postgres")
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, sourceName, connectionString)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownPG()
			teardownHGE()
		}
	})

	AfterEach(func() { teardown() })

	Context("metadata inconsistency status test", func() {
		It("Checks if the metadata is inconsistent or not", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "status"},
				WorkingDirectory: projectDirectory,
			})
			want := `metadata is consistent`
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring(want))
		})
	})

	Context("metadata inconsistency status test incase of inconsistent metadata", func() {
		It("Checks if the metadata is inconsistent or not", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply"},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "status"},
				WorkingDirectory: projectDirectory,
			})
			want := `metadata is inconsistent, use 'hasura metadata ic list' command to see the inconsistent objects`
			Eventually(session.Wait(timeout)).Should(Exit(1))
			Expect(session.Err.Contents()).Should(ContainSubstring(want))
		})
	})
})
