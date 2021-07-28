package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/hasura/graphql-engine/cli/v2/util"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura scripts update-project-v3", func() {
	var projectDirectory string
	var devEndpoint, stagingEndpoint, prodEndpoint string
	var teardown func()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		// create three hasura instances to mimic a environment promotion scenario
		devHasuraPort, teardownDev := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		devEndpoint = fmt.Sprintf("http://0.0.0.0:%s", devHasuraPort)
		stagingHasuraPort, teardownStaging := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		stagingEndpoint = fmt.Sprintf("http://0.0.0.0:%s", stagingHasuraPort)
		prodHasuraPort, teardownProd := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		prodEndpoint = fmt.Sprintf("http://0.0.0.0:%s", prodHasuraPort)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownProd()
			teardownStaging()
			teardownDev()
		}
	})
	AfterEach(func() { teardown() })

	It("update a config v2 project to config v3", func() {
		Context("sets up dev project", func() {
			// copy template project directory migrations to test project directory
			Expect(util.CopyDir("testdata/config-v2-test-project", projectDirectory)).Should(BeNil())
			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), devEndpoint)
			// apply migrations and metadata
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply"},
				WorkingDirectory: projectDirectory,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply"},
				WorkingDirectory: projectDirectory,
			})
		})
		Context("applies migrations to staging and production", func() {
			// apply migrations and metadata
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--endpoint", stagingEndpoint},
				WorkingDirectory: projectDirectory,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply", "--endpoint", stagingEndpoint},
				WorkingDirectory: projectDirectory,
			})
			// apply migrations and metadata
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--endpoint", prodEndpoint},
				WorkingDirectory: projectDirectory,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply", "--endpoint", prodEndpoint},
				WorkingDirectory: projectDirectory,
			})
		})
		Context("updates dev project to config v3", func() {
			// apply migrations and metadata
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"scripts", "update-project-v3", "--database-name", "default", "--force"},
				WorkingDirectory: projectDirectory,
			})
		})

		Context("applies metadata and migrations on staging hasura instance with auto state migration disabled", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply", "--endpoint", stagingEndpoint},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default", "--endpoint", stagingEndpoint, "--disable-auto-state-migration"},
				WorkingDirectory: projectDirectory,
			})

			Eventually(session.Err, 60).Should(Say(`.*error.*`))
			Eventually(session.Wait(timeout)).Should(Exit())
		})

		Context("applies metadata and migrations on staging hasura instance", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply", "--endpoint", stagingEndpoint},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default", "--endpoint", stagingEndpoint},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring("nothing to apply"))

			// This now should not trigger a state migration
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default", "--endpoint", stagingEndpoint, "--log-level", "debug"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring(`{"level":"debug","msg":"skipping state migration, found IsStateCopyCompleted: true Migrations: map[default:map[1620138136207:false 1620138146208:false 1620138161039:false 1620138169404:false 1620138179776:false 1620138189381:false 1620138199344:false]]"`))
		})
		Context("applies metadata and migrations on production hasura instance", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"metadata", "apply", "--endpoint", prodEndpoint},
				WorkingDirectory: projectDirectory,
			})
			session := testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default", "--endpoint", prodEndpoint, "--disable-auto-state-migration"},
				WorkingDirectory: projectDirectory,
			})

			Eventually(session.Err, 60).Should(Say(`.*error.*`))
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"scripts", "update-project-v3", "--database-name", "default", "--force", "--move-state-only"},
				WorkingDirectory: projectDirectory,
			})

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default", "--endpoint", prodEndpoint},
				WorkingDirectory: projectDirectory,
			})

			Eventually(session, timeout).Should(Exit(0))
			Expect(session.Err.Contents()).Should(ContainSubstring("nothing to apply"))
		})
	})
})
