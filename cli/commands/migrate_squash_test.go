package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura migrate squash", func() {
	testFromFlag := func(projectDirectory string, globalFlags []string, sourceName string) {
		toSquash := []string{
			"1588172669699_create_table_public_table30",
			"1588172669752_create_table_public_table31",
			"1588172669806_create_table_public_table32",
			"1588172669861_create_table_public_table33",
			"1588172669920_create_table_public_table34",
			"1588172669974_create_table_public_table35",
			"1588172670028_create_table_public_table36",
			"1588172670088_create_table_public_table37",
			"1588172670146_create_table_public_table38",
			"1588172670201_create_table_public_table39",
			"1588172670256_create_table_public_table40",
			"1588172670313_create_table_public_table41",
			"1588172670367_create_table_public_table42",
			"1588172670422_create_table_public_table43",
			"1588172670478_create_table_public_table44",
			"1588172670532_create_table_public_table45",
			"1588172670588_create_table_public_table46",
			"1588172670644_create_table_public_table47",
			"1588172670697_create_table_public_table48",
			"1588172670757_create_table_public_table49",
			"1588172670820_create_table_public_table50",
		}
		session := testutil.Hasura(testutil.CmdOpts{
			Args: append([]string{
				"migrate",
				"squash",
				"--from",
				// testdata/migrate-squash-test/migrations/1588172669699_create_table_public_table30
				"1588172669699",
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

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		// verify squashed migrations are deleted
		migrationsDirectory := filepath.Join(projectDirectory, "migrations", sourceName)
		for _, dir := range toSquash {
			Expect(filepath.Join(migrationsDirectory, dir)).ShouldNot(BeADirectory())
		}

		// verify squashed migrations are permanently deleted
		squashedMigrationsDirectory := filepath.Join(migrationsDirectory, "squashed_1588172669699_to_1588172670820")
		Expect(squashedMigrationsDirectory).ShouldNot(BeADirectory())

		// verify squashed migrations are deleted in statestore
		session = testutil.Hasura(testutil.CmdOpts{
			Args:             append([]string{"migrate", "status"}, globalFlags...),
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		for _, dir := range toSquash {
			// get version from dir name "1588172670820_create_table_public_table50"
			Expect(session.Out.Contents()).ToNot(ContainSubstring(dir))
		}

		// assert contents of squashed migration
		files, err := ioutil.ReadDir(migrationsDirectory)
		Expect(err).To(BeNil())
		Expect(files).NotTo(BeEmpty())
		for _, f := range files {
			if strings.Contains(f.Name(), "squashed") {
				gotUpSQL, err := ioutil.ReadFile(filepath.Join(migrationsDirectory, f.Name(), "up.sql"))
				Expect(err).To(BeNil())
				gotDownSQL, err := ioutil.ReadFile(filepath.Join(migrationsDirectory, f.Name(), "down.sql"))
				Expect(err).To(BeNil())

				wantUpSQL, err := ioutil.ReadFile("testdata/migrate-squash-test/want_from.up.sql")
				Expect(err).To(BeNil())
				wantDownSQL, err := ioutil.ReadFile("testdata/migrate-squash-test/want_from.down.sql")
				Expect(err).To(BeNil())

				Expect(string(gotUpSQL)).To(Equal(string(wantUpSQL)))
				Expect(string(gotDownSQL)).To(Equal(string(wantDownSQL)))

				break
			}
		}
	}
	testToFlag := func(projectDirectory string, globalFlags []string, sourceName string) {
		toSquash := []string{
			"1588172668232_create_table_public_table4",
			"1588172668287_create_table_public_table5",
			"1588172668342_create_table_public_table6",
			"1588172668394_create_table_public_table7",
			"1588172668471_create_table_public_table8",
			"1588172668531_create_table_public_table9",
		}
		session := testutil.Hasura(testutil.CmdOpts{
			Args: append([]string{
				"migrate",
				"squash",
				"--from",
				// testdata/migrate-squash-test/migrations/1588172668232_create_table_public_table4
				"1588172668232",
				"--to",
				"1588172668531",
			}, globalFlags...),
			WorkingDirectory: projectDirectory,
		})

		wantKeywordList := []string{
			"Squashing migrations from",
			"Created",
			"after squashing",
			"till",
		}

		Eventually(session, timeout).Should(Exit(0))
		for _, keyword := range wantKeywordList {
			Expect(session.Err.Contents()).Should(ContainSubstring(keyword))
		}

		// verify squashed migrations are deleted
		migrationsDirectory := filepath.Join(projectDirectory, "migrations", sourceName)
		for _, dir := range toSquash {
			Expect(filepath.Join(migrationsDirectory, dir)).ShouldNot(BeADirectory())
		}

		// verify squashed migrations are moved to squashed_1588172668232_1588172668531
		squashedMigrationsDirectory := filepath.Join(migrationsDirectory, "squashed_1588172668232_to_1588172668531")
		for _, migration := range toSquash {
			Expect(filepath.Join(squashedMigrationsDirectory, migration)).Should(BeADirectory())
		}

		// verify squashed migrations are deleted in statestore
		session = testutil.Hasura(testutil.CmdOpts{
			Args:             append([]string{"migrate", "status"}, globalFlags...),
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		for _, dir := range toSquash {
			// get version from dir name "1588172670820_create_table_public_table50"
			Expect(session.Out.Contents()).ToNot(ContainSubstring(dir))
		}

		// assert contents of squashed migration
		files, err := ioutil.ReadDir(migrationsDirectory)
		Expect(err).To(BeNil())
		Expect(files).NotTo(BeEmpty())
		for _, f := range files {
			if strings.Contains(f.Name(), "squashed") && strings.Contains(f.Name(), "1588172668531") {
				gotUpSQL, err := ioutil.ReadFile(filepath.Join(migrationsDirectory, f.Name(), "up.sql"))
				Expect(err).To(BeNil())
				gotDownSQL, err := ioutil.ReadFile(filepath.Join(migrationsDirectory, f.Name(), "down.sql"))
				Expect(err).To(BeNil())

				wantUpSQL, err := ioutil.ReadFile("testdata/migrate-squash-test/want_from_to.up.sql")
				Expect(err).To(BeNil())
				wantDownSQL, err := ioutil.ReadFile("testdata/migrate-squash-test/want_from_to.down.sql")
				Expect(err).To(BeNil())

				Expect(string(gotUpSQL)).To(Equal(string(wantUpSQL)))
				Expect(string(gotDownSQL)).To(Equal(string(wantDownSQL)))

				break
			}
		}
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
		copyMigrationsToProjectDirectory(projectDirectoryLatestConfigV3, "testdata/migrate-squash-test/migrations", "default")

		projectDirectoryLatestConfigV2 = testutil.RandDirName()
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryLatestConfigV2, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryLatestConfigV2, defaultConfigFilename), hgeEndpointLatest)
		copyMigrationsToProjectDirectory(projectDirectoryLatestConfigV2, "testdata/migrate-squash-test/migrations")

		projectDirectoryV13 = testutil.RandDirName()
		hgeEndPortV13, teardownHGEV13 := testutil.StartHasura(GinkgoT(), "hasura/graphql-engine:v1.3.3")
		hgeEndpointV13 := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPortV13)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryV13, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryV13, defaultConfigFilename), hgeEndpointV13)
		copyMigrationsToProjectDirectory(projectDirectoryV13, "testdata/migrate-squash-test/migrations")

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
		Context("config v3", func() {
			testFromFlag(projectDirectoryLatestConfigV3, []string{"--database-name", "default"}, "default")
			testToFlag(projectDirectoryLatestConfigV3, []string{"--database-name", "default"}, "default")
		})
		Context("config v2 (latest)", func() {
			testFromFlag(projectDirectoryLatestConfigV2, nil, "")
			testToFlag(projectDirectoryLatestConfigV2, nil, "")
		})
		Context("config v2 v1.3.3", func() {
			testFromFlag(projectDirectoryV13, nil, "")
			testToFlag(projectDirectoryV13, nil, "")
		})
	})
})
