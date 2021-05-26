package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("seed_create", func() {

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

	Context("seed create test", func() {
		upSql := `CREATE TABLE "public"."table1" ("id" serial NOT NULL, PRIMARY KEY ("id") );
		INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`

		downSql := `DROP TABLE "public"."table1";`

		It("should create a seed file inside seed/default ", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "table1", "--up-sql", upSql, "--down-sql", downSql, "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"seed", "create", "table_seed", "--from-table", "table1", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			Eventually(session, 60*60).Should(Exit(0))
			Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("created seed file successfully"))
		})
	})
})
