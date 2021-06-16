package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura migrate squash (config v3)", func() {
	test := func(projectDirectory string, globalFlags []string) {
		session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: append([]string{
				"migrate",
				"create",
				"schema_creation",
				"--up-sql",
				"create schema \"testing\";",
				"--down-sql",
				"drop schema \"testing\" cascade;",
			}, globalFlags...),
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, 60*40).Should(Exit(0))
		logs := string(session.Wait().Err.Contents())
		version := regexp.MustCompile(`"version":\d+`)
		matches := version.FindStringSubmatch(logs)
		session = testutil.Hasura(testutil.CmdOpts{
			Args: append([]string{
				"migrate",
				"squash",
				"--from",
				strings.Split(matches[0], ":")[1],
				"--delete-source",
			}, globalFlags...),
			WorkingDirectory: projectDirectory,
		})
		wantKeywordList := []string{
			"Squashing migrations from",
			"Created",
			"after squashing",
			"till",
		}

		Eventually(session, 60*40).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring(keyword))
		}
		// verify files were deleted
		v := strings.Split(matches[0], ":")[1]
		Expect(filepath.Join(projectDirectory, "migrations", "default", v)).ShouldNot(BeADirectory())
		// verify squashed migrations are deleted in statestore
		session = testutil.Hasura(testutil.CmdOpts{
			Args:             append([]string{"migrate", "status"}, globalFlags...),
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, 60).Should(Exit(0))
		Eventually(session.Out.Contents()).ShouldNot(ContainSubstring(v))
	}

	var projectDirectoryLatestConfigV3, projectDirectoryLatestConfigV2, projectDirectoryV13 string
	var teardown func()
	BeforeEach(func() {
		projectDirectoryLatestConfigV3 = testutil.RandDirName()
		hgeEndPortLatest, teardownHGELatest := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpointLatest := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortLatest)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryLatestConfigV3},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryLatestConfigV3, defaultConfigFilename), hgeEndpointLatest)

		projectDirectoryLatestConfigV2 = testutil.RandDirName()
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryLatestConfigV2, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryLatestConfigV2, defaultConfigFilename), hgeEndpointLatest)

		projectDirectoryV13 = testutil.RandDirName()
		hgeEndPortV13, teardownHGEV13 := testutil.StartHasura(GinkgoT(), "hasura/graphql-engine:v1.3.3")
		hgeEndpointV13 := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortV13)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryV13, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryV13, defaultConfigFilename), hgeEndpointV13)

		teardown = func() {
			os.RemoveAll(projectDirectoryLatestConfigV3)
			os.RemoveAll(projectDirectoryLatestConfigV2)
			os.RemoveAll(projectDirectoryV13)
			teardownHGELatest()
			teardownHGEV13()
		}
	})

	AfterEach(func() { teardown() })

	It("can squash the migrations in local project dir", func() {
		Context("config v3", func() { test(projectDirectoryLatestConfigV3, []string{"--database-name", "default"}) })
		Context("config v2 (latest)", func() { test(projectDirectoryLatestConfigV2, nil) })
		Context("config v2 v1.3.3", func() { test(projectDirectoryV13, nil) })
	})
})
