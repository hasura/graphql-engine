package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/Pallinder/go-randomdata"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura init --endpoint (config v3)", func() {
	var projectDirectory string
	var teardown func()
	var hgeEndpoint string
	sourceName := randomdata.SillyName()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, "default", connectionString)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("should create version 3 metadata directory", func() {
		err := os.RemoveAll(projectDirectory)
		Expect(err).To(BeNil())

		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{"init", projectDirectory},
		})
		Eventually(session, timeout).Should(Exit(0))

		wantKeywordList := []string{
			fmt.Sprintf("cd %s", projectDirectory),
			"hasura console",
		}
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		// check that required files are present
		wantFilesList := []string{
			filepath.Join(projectDirectory, "metadata", "databases", "databases.yaml"),
		}
		for _, file := range wantFilesList {
			_, err := ioutil.ReadFile(file)
			Expect(err).To(BeNil())
		}

		// check contents of metadata/version.yaml
		gotMetadataVersion, err := ioutil.ReadFile(filepath.Join(projectDirectory, "metadata", "version.yaml"))
		Expect(err).To(BeNil())
		goldenMetadataVersion, err := ioutil.ReadFile("testdata/init_test/config-v3/metadata/version.golden.yaml")
		Expect(err).To(BeNil())

		Expect(gotMetadataVersion).Should(MatchYAML(goldenMetadataVersion))
	})

	It("should create directory with metadata and migrations from server", func() {
		Context("create migrations and apply those on server", func() {
			testMigrateApply(projectDirectory, []string{"--database-name", sourceName})
		})
		Context("test init --endpoint for config v3", func() {
			err := os.RemoveAll(projectDirectory)
			Expect(err).To(BeNil())
			session := testutil.Hasura(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--endpoint", hgeEndpoint, "--fetch"},
			})

			wantKeywordList := []string{
				fmt.Sprintf("cd %s", projectDirectory),
				"hasura console",
				"Metadata exported",
				"migrations applied",
			}
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			fileInfos, err := os.ReadDir(filepath.Join(projectDirectory, "migrations", sourceName))
			Expect(err).To(BeNil())
			Expect(len(fileInfos)).Should(BeEquivalentTo(1))
			upMigrationsContent, err := ioutil.ReadFile(filepath.Join(projectDirectory, "migrations", sourceName, fileInfos[0].Name(), "up.sql"))
			Expect(err).To(BeNil())
			Expect(upMigrationsContent).ShouldNot(ContainSubstring("hdb_catalog"))
			Expect(upMigrationsContent).ShouldNot(ContainSubstring("hdb_views"))
		})

	})

})

var _ = Describe("hasura init --endpoint (config v2)", func() {
	var projectDirectory string
	var teardown func()
	var hgeEndpoint string

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		copyTestConfigV2Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	It("should create version 2 metadata directory", func() {
		err := os.RemoveAll(projectDirectory)
		Expect(err).To(BeNil())

		session := testutil.Hasura(testutil.CmdOpts{
			Args: []string{"init", projectDirectory, "--version", "2"},
		})
		Eventually(session, timeout).Should(Exit(0))

		wantKeywordList := []string{
			fmt.Sprintf("cd %s", projectDirectory),
			"hasura console",
		}
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		// check that required files are present
		wantFilesList := []string{
			filepath.Join(projectDirectory, "metadata", "functions.yaml"),
			filepath.Join(projectDirectory, "metadata", "tables.yaml"),
		}
		for _, file := range wantFilesList {
			_, err := ioutil.ReadFile(file)
			Expect(err).To(BeNil())
		}

		// check contents of metadata/version.yaml
		gotMetadataVersion, err := ioutil.ReadFile(filepath.Join(projectDirectory, "metadata", "version.yaml"))
		Expect(err).To(BeNil())
		goldenMetadataVersion, err := ioutil.ReadFile("testdata/init_test/config-v2/metadata/version.golden.yaml")
		Expect(err).To(BeNil())

		Expect(gotMetadataVersion).Should(MatchYAML(goldenMetadataVersion))
	})

	It("should create directory with metadata and migrations from server", func() {
		Context("create migrations and apply those on server", func() {
			testMigrateApply(projectDirectory, nil)
		})
		Context("test init --endpoint for config v2", func() {
			err := os.RemoveAll(projectDirectory)
			Expect(err).To(BeNil())
			session := testutil.Hasura(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--endpoint", hgeEndpoint, "--version", "2", "--fetch"},
			})

			wantKeywordList := []string{
				fmt.Sprintf("cd %s", projectDirectory),
				"hasura console",
				"Metadata exported",
				"migrations applied",
			}
			Eventually(session, timeout).Should(Exit(0))
			for _, keyword := range wantKeywordList {
				Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
			}
			fileInfos, err := os.ReadDir(filepath.Join(projectDirectory, "migrations"))
			Expect(err).To(BeNil())
			Expect(len(fileInfos)).Should(BeEquivalentTo(1))
			upMigrationsContent, err := ioutil.ReadFile(filepath.Join(projectDirectory, "migrations", fileInfos[0].Name(), "up.sql"))
			Expect(err).To(BeNil())
			Expect(string(upMigrationsContent)).ShouldNot(ContainSubstring("hdb_catalog"))
			Expect(string(upMigrationsContent)).ShouldNot(ContainSubstring("hdb_views"))
		})

	})

})
