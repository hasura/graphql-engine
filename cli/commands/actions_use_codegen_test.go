package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("actions_use_codegen", func() {

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

	Context("actions use codegen tests", func() {
		It("should change the config.yaml file and create the nodejs-express directory ", func() {
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"actions", "use-codegen", "--framework", "nodejs-express", "--output-dir", "codegen", "--with-starter-kit", "true"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*Starter kit cloned at*.",
				".*Codegen configuration updated in config.yaml*.",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session, 60*40).Should(Say(keyword))
			}
			Eventually(session, 60*40).Should(Exit(0))
		})
	})
})
