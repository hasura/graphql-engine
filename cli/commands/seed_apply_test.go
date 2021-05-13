package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("seed_apply", func() {

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

	Context("seed apply test", func() {
		up_sql := `CREATE TABLE "public"."table1" ("id" serial NOT NULL, PRIMARY KEY ("id") );`

		data := `INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`

		down_sql := `DROP TABLE "public"."table1";`

		It("should apply the seed file inside seed/default ", func() {
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "table1", "--up-sql", up_sql, "--down-sql", down_sql, "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			err := os.Mkdir(filepath.Join(filepath.Join(dirName, "seeds"), "default"), 0755)
			if err != nil {
				fmt.Println(err, "error creating default directory in seeds")
			}
			filePointer, err := os.Create(filepath.Join(filepath.Join(filepath.Join(dirName, "seeds"), "default"), "table_seed.sql"))
			if err != nil {
				fmt.Println(err, "not able to create table_seed.sql file")
			}
			_, err = filePointer.WriteString(data)
			if err != nil {
				fmt.Println(err, "not able to write into table_seed.sql file")
			}
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"seed", "apply", "--file", "table_seed.sql", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*Seeds planted*.",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session, 60*60).Should(Say(keyword))
			}
			Eventually(session, 60*60).Should(Exit(0))
		})
	})
})
