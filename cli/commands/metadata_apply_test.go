package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/Pallinder/go-randomdata"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var inconsistent_query_collections = `
- name: sample-collection
  definition:
    queries:
      - name: test
        query: |-
          query test {
            books {
              id
              author_id
              title
            }
          }
      - name: test2
        query: |-
          query test2 {
              authors{
                  id
                  author_name
              }
          }
`

var commonMetadataCommandsTest = func(projectDirectory string) {
	Context("check the behavior --dry-run", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"metadata", "apply", "--dry-run"},
			WorkingDirectory: projectDirectory,
		})
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "diff"},
			WorkingDirectory: projectDirectory,
		})

		Eventually(session, timeout).Should(Exit(0))
		stdout := session.Out.Contents()
		Expect(stdout).Should(ContainSubstring("tables"))
	})
	Context("should apply metadata to server", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		Expect(session.Err.Contents()).Should(ContainSubstring("Metadata applied"))
	})
	Context("Should apply metadata to server with inconsistencies allowed", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply", "--disallow-inconsistent-metadata=false",},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		Expect(session.Err.Contents()).Should(ContainSubstring("Metadata applied"))
	})
	Context("apply metadata to server and it should output the metadata of project to stdout", func() {
		projectSession := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply", "--output", "json"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(projectSession, timeout).Should(Exit(0))
		Eventually(isJSON(projectSession.Out.Contents())).Should(BeTrue())
		projectStdout := projectSession.Out.Contents()

		serverSession := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "export", "--output", "json"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(serverSession, timeout).Should(Exit(0))
		Eventually(isJSON(serverSession.Out.Contents())).Should(BeTrue())
		serverStdout := serverSession.Out.Contents()
		Expect(serverStdout).Should(MatchJSON(projectStdout))
	})
	Context("metadata modes", func() {
		editMetadataFileInConfig(filepath.Join(projectDirectory, defaultConfigFilename), "metadata.random")
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).ShouldNot(Exit(0))

		editMetadataFileInConfig(filepath.Join(projectDirectory, defaultConfigFilename), "metadata.json")
		session = testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).ShouldNot(Exit(0))
	})
}

var inconsistentMetadataTestsForConfigV3 = func(projectDirectory string) {
	Context("Should apply metadata to server with inconsistencies allowed", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		Expect(session.Err.Contents()).Should(ContainSubstring("Metadata applied"))
	})

	Context("Should not apply metadata to server with inconsistencies", func() {
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "apply", "--disallow-inconsistent-metadata"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(1))
		Expect(session.Err.Contents()).Should(ContainSubstring("cannot continue due to inconsistent metadata"))
	})
}

var testConfigV2 = func(projectDirectory string) {
	Context("apply migrations", func() {
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"migrate", "apply"},
			WorkingDirectory: projectDirectory,
		})
	})
	commonMetadataCommandsTest(projectDirectory)
}

var verifyRequestTransformsMetadataIsApplied = func(hgeEndpoint string) {
	response := assertHGEAPIRequestSucceedsAndGetResponseBody(hgeEndpoint, "v1/metadata", strings.NewReader(`{"type": "export_metadata", "args": {}}`))
	Expect(string(response)).To(ContainSubstring("request_transform"))
	Expect(string(response)).To(ContainSubstring("$body.input.arg1.username"))
	Expect(string(response)).To(ContainSubstring("actions_test_comment"))
}
var _ = Describe("hasura metadata apply (config v3)", func() {
	var projectDirectory, hgeEndpoint string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		sourceName := randomdata.SillyName()
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, sourceName, connectionString)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownPG()
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("metadata apply", func() {
		commonMetadataCommandsTest(projectDirectory)
		verifyRequestTransformsMetadataIsApplied(hgeEndpoint)
	})

	It("Make metadata inconsistent", func() {    
		file := fmt.Sprintf("%s/metadata/query_collections.yaml", projectDirectory)
		err := os.WriteFile(file, []byte(inconsistent_query_collections), 0666)
		Expect(err).To(BeNil())
		Context("inconsistent metadata apply", func ()  {
			inconsistentMetadataTestsForConfigV3(projectDirectory)
		})
	})
})

var _ = Describe("hasura metadata apply (config v2)", func() {
	var projectDirectoryLatest, projectDirectoryV13 string
	var hgeEndpoint string
	var teardown func()
	BeforeEach(func() {
		projectDirectoryLatest = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		copyTestConfigV2Project(projectDirectoryLatest)
		editEndpointInConfig(filepath.Join(projectDirectoryLatest, defaultConfigFilename), hgeEndpoint)

		projectDirectoryV13 = testutil.RandDirName()
		hgeEndPortV13, teardownHGEV13 := testutil.StartHasura(GinkgoT(), "hasura/graphql-engine:v1.3.3")
		hgeEndpointV13 := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortV13)
		copyTestConfigV2Project(projectDirectoryV13)
		editEndpointInConfig(filepath.Join(projectDirectoryV13, defaultConfigFilename), hgeEndpointV13)

		teardown = func() {
			os.RemoveAll(projectDirectoryLatest)
			os.RemoveAll(projectDirectoryV13)
			teardownHGE()
			teardownHGEV13()
		}
	})

	AfterEach(func() { teardown() })

	It("metadata apply", func() {
		testConfigV2(projectDirectoryLatest)
		verifyRequestTransformsMetadataIsApplied(hgeEndpoint)
		testConfigV2(projectDirectoryV13)
	})
})
