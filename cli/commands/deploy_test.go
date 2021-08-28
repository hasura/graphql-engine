package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/Pallinder/go-randomdata"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var commonDeployCommandsTest = func(projectDirectory string, dbName string) {
	Context("should apply metadata, run migrations and reload metadata", func() {
		// Run Deploy
		session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"deploy"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(session, timeout).Should(Exit(0))
		deploy_output := session.Err.Contents()
		Expect(deploy_output).Should(ContainSubstring("Metadata applied"))
		Expect(deploy_output).Should(ContainSubstring("migrations applied on database"))
		Expect(deploy_output).Should(ContainSubstring("Metadata reloaded"))

		// Ensure metadata is applied
		metadata_session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"metadata", "inconsistency", "status"},
			WorkingDirectory: projectDirectory,
		})
		Eventually(metadata_session, timeout).Should(Exit(0))
		metadata_output := string(metadata_session.Err.Contents())
		Expect(metadata_output).Should(ContainSubstring("metadata is consistent"))

		// Ensure migrations are applied
		migrate_session := testutil.Hasura(testutil.CmdOpts{
			Args:             []string{"migrate", "status", "--database-name", dbName},
			WorkingDirectory: projectDirectory,
		})
		Eventually(migrate_session, timeout).Should(Exit(0))
		migrate_output := string(migrate_session.Out.Contents())
		Expect(migrate_output).Should(ContainSubstring("Present        Present"))
	})
}

var _ = Describe("hasura deploy", func() {
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
		commonDeployCommandsTest(projectDirectory, sourceName)
	})
})
