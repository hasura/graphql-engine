package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura migrate status", func() {
	var dirName string
	var session *Session
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
			session.Kill()
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() { teardown() })

	Context("migrate status test", func() {
		It("should show the status of migrations between local and server ", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*VERSION*.",
				".*SOURCE STATUS*.",
				".*DATABASE STATUS*.",
				".*schema_creation*.",
				".*Present        Not Present*.",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Out).Should(Say(keyword))
			}
			Eventually(session, timeout).Should(Exit(0))
		})
	})

})
