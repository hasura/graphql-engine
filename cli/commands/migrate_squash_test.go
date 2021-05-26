package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("migrate_squash", func() {

	var dirName string
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
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	Context("migrate squash test", func() {
		It("should squash the migrations in local project dir", func() {
			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*40).Should(Exit(0))
			logs := string(session.Wait().Err.Contents())
			version := regexp.MustCompile(`"version":\d+`)
			matches := version.FindStringSubmatch(logs)
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "squash", "--database-name", "default", "--from", strings.Split(matches[0], ":")[1], "--delete-source"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*Squashing migrations from*.",
				".*Created*.",
				".*after squashing*.",
				".* till .",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session.Err, 60*40).Should(Say(keyword))
			}
			Eventually(session, 60*40).Should(Exit(0))
			// verify files were deleted
			v := strings.Split(matches[0], ":")[1]
			Expect(filepath.Join(dirName, "migrations", "default", v)).ShouldNot(BeADirectory())
			// verify squashed migrations are deleted in statestore
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60).Should(Exit(0))
			Eventually(session.Out.Contents()).ShouldNot(ContainSubstring(v))
		})
	})
})
