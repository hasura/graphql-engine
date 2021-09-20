package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura actions use-codegen", func() {

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

	AfterEach(func() { teardown() })

	Context("actions use codegen tests", func() {
		It("should change the config.yaml file and create the nodejs-express directory ", func() {
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"actions", "use-codegen", "--framework", "nodejs-express", "--output-dir", "codegen", "--with-starter-kit", "true"},
				WorkingDirectory: projectDirectory,
			})
			wantKeywordList := []string{
				"Starter kit cloned at",
				"Codegen configuration updated in config.yaml",
			}

			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			configPath := filepath.Join(projectDirectory, "config.yaml")
			contents, err := ioutil.ReadFile(configPath)
			Expect(err).To(BeNil())
			wantKeywordList = []string{
				"framework: nodejs-express",
				"output_dir: codegen",
			}

			for _, keyword := range wantKeywordList {
				Eventually(contents).Should(ContainSubstring(keyword))
			}

			Expect(filepath.Join(projectDirectory, "nodejs-express")).To(BeADirectory())

		})
	})
})
