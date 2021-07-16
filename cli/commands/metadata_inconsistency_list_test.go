package commands

import (
	"fmt"
	"github.com/Pallinder/go-randomdata"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura metadata inconsistency list", func() {
	var projectDirectory string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		sourceName := randomdata.SillyName()
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, sourceName, connectionString)

		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
		}
	})

	AfterEach(func() { teardown() })
	var matcher = func(out []byte) {
		want := []string{"genres", "albums", "media_types", "playlists", "artists", "tracks", "playlist_track"}
		for _, v := range want {
			Expect(out).To(ContainSubstring(v))
		}
	}

	It("should list inconsistent metadata objects", func() {
		Context("Lists all inconsistent objects in table format", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "list"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, 60*40).Should(Exit(0))
			matcher(session.Wait().Out.Contents())
		})
		Context("Lists all inconsistent objects in json format", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "list", "-o", "json"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, 60*40).Should(Exit(0))
			stdout := session.Wait().Out.Contents()
			Eventually(isJSON(stdout)).Should(BeTrue())
			matcher(stdout)
		})
	})
})
