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

var testSeedCreate = func(projectDirectory string, globalFlags []string) {
	upSql := `CREATE TABLE "public"."table1" ("id" serial NOT NULL, PRIMARY KEY ("id") );
		INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`

	downSql := `DROP TABLE "public"."table1";`

	testutil.RunCommandAndSucceed(testutil.CmdOpts{
		Args:             append([]string{"migrate", "create", "table1", "--up-sql", upSql, "--down-sql", downSql}, globalFlags...),
		WorkingDirectory: projectDirectory,
	})
	testutil.RunCommandAndSucceed(testutil.CmdOpts{
		Args:             append([]string{"migrate", "apply"}, globalFlags...),
		WorkingDirectory: projectDirectory,
	})
	session := testutil.Hasura(testutil.CmdOpts{
		Args:             append([]string{"seed", "create", "table_seed", "--from-table", "table1"}, globalFlags...),
		WorkingDirectory: projectDirectory,
	})
	Eventually(session, timeout).Should(Exit(0))
	Expect(session.Err.Contents()).Should(ContainSubstring("created seed file successfully"))
}

var _ = Describe("hasura seed create", func() {
	Context("config v3", func() {
		var teardown func()
		var projectDirectory string
		var sourceName string
		BeforeEach(func() {
			projectDirectory = testutil.RandDirName()
			hgeEndPort, teardownHGE := testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraDockerImage)
			hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
			sourceName = randomdata.SillyName()
			_, teardownPG := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, sourceName, "postgres")

			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectory},
			})
			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

			teardown = func() {
				teardownPG()
				teardownHGE()
				os.RemoveAll(projectDirectory)
			}
		})
		AfterEach(func() { teardown() })
		It("can create seeds in config v3", func() {
			testSeedCreate(projectDirectory, []string{"--database-name", sourceName})
		})
	})
	Context("config v2", func() {
		var teardown func()
		var projectDirectoryConfigV2 string
		BeforeEach(func() {
			projectDirectoryConfigV2 = testutil.RandDirName()
			hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
			hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectoryConfigV2, "--version", "2"},
			})
			editEndpointInConfig(filepath.Join(projectDirectoryConfigV2, defaultConfigFilename), hgeEndpoint)

			teardown = func() {
				teardownHGE()
				os.RemoveAll(projectDirectoryConfigV2)
			}
		})
		AfterEach(func() { teardown() })
		It("can create seeds in config v2", func() {
			testSeedCreate(projectDirectoryConfigV2, nil)
		})
	})
})
