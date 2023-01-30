package commands

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/spf13/viper"

	"github.com/hasura/graphql-engine/cli/v2"

	"github.com/Pallinder/go-randomdata"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var commonDeployCommandsTest = func(projectDirectory string, databaseFlags ...string) {
	Context("should apply metadata, run migrations and reload metadata", func() {
		// Run Deploy
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"deploy"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		deployOutput := session.Err.Contents()
		Expect(deployOutput).Should(ContainSubstring("Metadata applied"))
		Expect(deployOutput).Should(ContainSubstring("migrations applied"))
		// incase of config_v3
		if len(databaseFlags) != 0 {
			Expect(deployOutput).Should(ContainSubstring("Metadata reloaded"))
		}

		// Ensure metadata is applied
		metadataSession := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "inconsistency", "status"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(metadataSession, timeout).Should(Exit(0))
		metadataOutput := string(metadataSession.Err.Contents())
		Expect(metadataOutput).Should(ContainSubstring("metadata is consistent"))

		// Ensure migrations are applied
		migrateSession := testutil.Hasura(testutil.CmdOpts{
			Args:             append([]string{"migrate", "status"}, databaseFlags...),
			WorkingDirectory: projectDirectory,
		})
		Eventually(migrateSession, timeout).Should(Exit(0))
		migrateOutput := string(migrateSession.Out.Contents())
		Expect(migrateOutput).Should(ContainSubstring("Present        Present"))
	})
}

var _ = Describe("hasura deploy config v3", func() {
	var projectDirectory string
	var teardown func()
	sourceName := randomdata.SillyName()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

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

	It("deploy", func() {
		commonDeployCommandsTest(projectDirectory, "--database-name", sourceName)
	})
})

var _ = Describe("hasura deploy config v2", func() {
	var projectDirectory string
	var teardown func()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
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

	It("deploy", func() {
		commonDeployCommandsTest(projectDirectory)
	})
})

var _ = Describe("fsm state transitions: config v2", func() {

	var projectDirectory string
	var teardown, teardownHGE func()

	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		var hgeEndPort string
		hgeEndPort, teardownHGE = testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		copyTestConfigV2Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			os.RemoveAll(projectDirectory)
		}
	})

	AfterEach(func() {
		teardown()
	})
	It("should make correct state transitions", func() {
		ctx := &deployCtx{
			err: nil,
		}
		ctx.ec = cli.NewExecutionContext()
		ctx.ec.Viper = viper.New()
		ctx.ec.ExecutionDirectory = projectDirectory
		//ctx.ec.LogLevel = "DEBUG"
		Expect(ctx.ec.Prepare()).Should(BeNil())
		//ctx.ec.Stderr = io.Discard
		Expect(ctx.ec.Validate()).Should(BeNil())
		ctx.logger = ctx.ec.Logger

		// bad sql migration
		configV2FSM := newConfigV2DeployFSM()
		migrationFilePath := filepath.Join(projectDirectory, "migrations", "1620138136207_create_table_public_t1/up.sql")
		upSQL, err := ioutil.ReadFile(migrationFilePath)
		Expect(err).To(BeNil())
		badUpSQL := []byte(string(upSQL) + "SOME GARBAGE;")
		Expect(ioutil.WriteFile(migrationFilePath, badUpSQL, 0755)).To(BeNil())

		Expect(configV2FSM.SendEvent(applyMigrations, ctx)).To(BeNil())
		Expect(configV2FSM.Current).Should(Equal(failedOperation))
		Expect(configV2FSM.Previous).Should(Equal(applyingMigrationsFailed))
		ctx.ec.Logger.Debugf("error: %v", ctx.err)
		Expect(ctx.err).Should(Not(BeNil()))
		// reset file contents
		Expect(ioutil.WriteFile(migrationFilePath, upSQL, 0755)).To(BeNil())

		// bad metadata yaml file
		tablesYamlFilepath := filepath.Join(projectDirectory, "metadata", "tables.yaml")
		tablesYaml, err := ioutil.ReadFile(tablesYamlFilepath)
		Expect(err).To(BeNil())

		// user error in tables.yaml
		configV2FSM = newConfigV2DeployFSM()
		badTablesYaml := []byte(`some: key`)
		Expect(ioutil.WriteFile(tablesYamlFilepath, badTablesYaml, 0755)).To(BeNil())
		Expect(configV2FSM.SendEvent(applyMigrations, ctx)).To(BeNil())
		Expect(configV2FSM.Current).Should(Equal(failedOperation))
		Expect(configV2FSM.Previous).Should(Equal(applyingMetadataFailed))
		ctx.ec.Logger.Debugf("error: %v", ctx.err)
		Expect(ctx.err).Should(Not(BeNil()))

		// validation error from server
		configV2FSM = newConfigV2DeployFSM()
		badTablesYaml = []byte(`
- table:
    schema: public
# this should be a string not a number
    name: 1
`)
		Expect(ioutil.WriteFile(tablesYamlFilepath, badTablesYaml, 0755)).To(BeNil())
		Expect(configV2FSM.SendEvent(applyMigrations, ctx)).To(BeNil())
		ctx.ec.Logger.Debugf("error: %v", ctx.err)
		Expect(configV2FSM.Current).Should(Equal(failedOperation))
		Expect(configV2FSM.Previous).Should(Equal(applyingMetadataFailed))
		Expect(ctx.err).Should(Not(BeNil()))
		// reset file contents
		Expect(ioutil.WriteFile(tablesYamlFilepath, tablesYaml, 0755)).To(BeNil())

		// happy path
		configV2FSM = newConfigV2DeployFSM()
		Expect(configV2FSM.SendEvent(applyMigrations, ctx)).To(BeNil())
		Expect(configV2FSM.Current).Should(Equal(applyingSeeds))

		// unreachable HGE
		teardownHGE()
		configV2FSM = newConfigV2DeployFSM()
		Expect(configV2FSM.SendEvent(applyMigrations, ctx)).To(BeNil())
		Expect(configV2FSM.Current).Should(Equal(failedOperation))
	})
})

var _ = Describe("fsm state transitions: config v3", func() {
	var projectDirectory string
	var teardown, teardownHGE func()

	sourceName := randomdata.SillyName()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		var hgeEndPort string
		hgeEndPort, teardownHGE = testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)

		connectionString, teardownPG := testutil.StartPGContainer(GinkgoT())
		testutil.AddPGSourceToHasura(GinkgoT(), hgeEndpoint, connectionString, sourceName)
		copyTestConfigV3Project(projectDirectory)
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		editSourceNameInConfigV3ProjectTemplate(projectDirectory, sourceName, connectionString)

		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownPG()
		}
	})

	AfterEach(func() {
		teardown()
	})
	It("should make correct state transitions", func() {
		ctx := &deployCtx{
			err: nil,
		}
		ctx.ec = cli.NewExecutionContext()
		ctx.ec.Viper = viper.New()
		ctx.ec.ExecutionDirectory = projectDirectory
		//ctx.ec.LogLevel = "DEBUG"
		Expect(ctx.ec.Prepare()).Should(BeNil())
		ctx.ec.Stderr = io.Discard
		Expect(ctx.ec.Validate()).Should(BeNil())
		ctx.logger = ctx.ec.Logger

		// bad sql migration
		configV3DeployFSM := newConfigV3DeployFSM()
		migrationFilePath := filepath.Join(projectDirectory, "migrations", sourceName, "1622047079431_chinook/up.sql")
		upSQL, err := ioutil.ReadFile(migrationFilePath)
		Expect(err).To(BeNil())
		badUpSQL := []byte("SOME GARBAGE;")
		Expect(ioutil.WriteFile(migrationFilePath, badUpSQL, 0755)).To(BeNil())

		Expect(configV3DeployFSM.SendEvent(applyInitialMetadata, ctx)).To(BeNil())
		Expect(configV3DeployFSM.Current).Should(Equal(failedOperation))
		Expect(configV3DeployFSM.Previous).Should(Equal(applyingMigrationsFailed))
		ctx.ec.Logger.Debugf("error: %v", ctx.err)
		Expect(ctx.err).Should(Not(BeNil()))
		// reset file contents
		Expect(ioutil.WriteFile(migrationFilePath, upSQL, 0755)).To(BeNil())

		// bad metadata yaml file
		tablesYamlFilepath := filepath.Join(projectDirectory, "metadata", "databases", sourceName, "tables", "tables.yaml")
		tablesYaml, err := ioutil.ReadFile(tablesYamlFilepath)
		Expect(err).To(BeNil())

		// user error in tables.yaml
		configV3DeployFSM = newConfigV3DeployFSM()
		badTablesYaml := []byte(`some: key`)
		Expect(ioutil.WriteFile(tablesYamlFilepath, badTablesYaml, 0755)).To(BeNil())
		Expect(configV3DeployFSM.SendEvent(applyInitialMetadata, ctx)).To(BeNil())
		Expect(configV3DeployFSM.Current).Should(Equal(failedOperation))
		Expect(configV3DeployFSM.Previous).Should(Equal(applyingInitialMetadataFailed))
		ctx.ec.Logger.Debugf("error: %v", ctx.err)
		Expect(ctx.err).Should(Not(BeNil()))

		// validation error from server
		configV3DeployFSM = newConfigV3DeployFSM()
		badTablesYaml = []byte(`
- table:
    schema: public
# this should be a string not a number
    name: 1
`)
		Expect(ioutil.WriteFile(tablesYamlFilepath, badTablesYaml, 0755)).To(BeNil())
		Expect(configV3DeployFSM.SendEvent(applyInitialMetadata, ctx)).To(BeNil())
		ctx.ec.Logger.Debugf("error: %v", ctx.err)
		Expect(configV3DeployFSM.Current).Should(Equal(failedOperation))
		Expect(configV3DeployFSM.Previous).Should(Equal(applyingInitialMetadataFailed))
		Expect(ctx.err).Should(Not(BeNil()))
		// reset file contents
		Expect(ioutil.WriteFile(tablesYamlFilepath, tablesYaml, 0755)).To(BeNil())

		// happy path
		configV3DeployFSM = newConfigV3DeployFSM()
		Expect(configV3DeployFSM.SendEvent(applyInitialMetadata, ctx)).To(BeNil())
		Expect(configV3DeployFSM.Current).Should(Equal(applyingSeeds))

		// unreachable HGE
		teardownHGE()
		configV3DeployFSM = newConfigV3DeployFSM()
		Expect(configV3DeployFSM.SendEvent(applyInitialMetadata, ctx)).To(BeNil())
		Expect(configV3DeployFSM.Current).Should(Equal(failedOperation))
	})
})
