package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("migrate_squash", func() {

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

	Context("migrate squash test", func() {
		It("should squash the migrations in local project dir", func() {
			out := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			contents := string(out.Out.Contents())
			contents_list := strings.Split(contents, ",")
			last := contents_list[len(contents_list)-1]
			version_no := last[10:23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "squash", "--database-name", "default", "--from", version_no, "--delete-source"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*Squashing migrations from*.",
				".*Created*.",
				".*after squashing*.",
				".* till .",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session, 60*40).Should(Say(keyword))
			}
			Eventually(session, 60*40).Should(Exit(0))
		})
	})
})
