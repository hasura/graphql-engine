package commands

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var test = func(projectDirectory string, globalFlags []string) {
	upSql := `CREATE TABLE "public"."table1" ("id" serial NOT NULL, PRIMARY KEY ("id") );`
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
		Args:             append([]string{"seed", "apply", "--file", "table_seed.sql"}, globalFlags...),
		WorkingDirectory: projectDirectory,
	})
	Eventually(session, 60*60).Should(Exit(0))
	Eventually(session.Wait().Err.Contents()).Should(ContainSubstring("Seeds planted"))
}

var _ = Describe("hasura seed apply", func() {

	var projectDirectoryLatest, projectDirectoryConfigV2 string
	var teardown func()
	var hgeEndpoint string
	BeforeEach(func() {
		hgeEndPort, teardownHGE := testutil.StartHasura(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint = fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		teardown = func() {
			teardownHGE()
		}
	})

	AfterEach(func() { teardown() })

	It("can apply seeds in config v3", func() {
		projectDirectoryLatest = testutil.RandDirName()
		defer os.RemoveAll(projectDirectoryLatest)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryLatest},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryLatest, defaultConfigFilename), hgeEndpoint)

		err := os.MkdirAll(filepath.Join(filepath.Join(projectDirectoryLatest, "seeds"), "default"), 0755)
		if err != nil {
			fmt.Println(err, "error creating default directory in seeds")
		}
		file, err := os.Create(filepath.Join(projectDirectoryLatest, "seeds", "default", "table_seed.sql"))
		if err != nil {
			fmt.Println(err, "not able to create table_seed.sql file")
		}
		data := `INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`

		_, err = file.WriteString(data)
		Expect(err).To(BeNil())
		test(projectDirectoryLatest, []string{"--database-name", "default"})
	})

	It("can apply seeds in config v2", func() {
		projectDirectoryConfigV2 = testutil.RandDirName()
		defer os.RemoveAll(projectDirectoryConfigV2)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryConfigV2, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryConfigV2, defaultConfigFilename), hgeEndpoint)

		err := os.MkdirAll(filepath.Join(filepath.Join(projectDirectoryConfigV2, "seeds")), 0755)
		file, err := os.Create(filepath.Join(projectDirectoryConfigV2, "seeds", "table_seed.sql"))
		Expect(err).To(BeNil())

		data := `INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`

		_, err = file.WriteString(data)
		Expect(err).To(BeNil())
		test(projectDirectoryConfigV2, nil)
	})
})
