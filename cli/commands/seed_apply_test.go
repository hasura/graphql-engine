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

var test = func(projectDirectory string, globalFlags []string, databaseKind string) {
	var upSql, downSql string
	if databaseKind == "mssql" {
		upSql = `CREATE SCHEMA test` + "\n" + `CREATE TABLE "test"."table1" ("id" int NOT NULL, PRIMARY KEY ("id") );`
		downSql = `DROP TABLE "test"."table1";` + "\n" + `DROP SCHEMA test`
	} else {
		upSql = `CREATE TABLE "public"."table1" ("id" serial NOT NULL, PRIMARY KEY ("id") );`
		downSql = `DROP TABLE "public"."table1";`
	}

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
	Eventually(session, timeout).Should(Exit(0))
	Expect(session.Err.Contents()).Should(ContainSubstring("Seeds planted"))
}

var _ = Describe("seed apply (config v2)", func() {
	var projectDirectoryConfigV2 string
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

	It("can apply seeds in config v2", func() {
		projectDirectoryConfigV2 = testutil.RandDirName()
		defer os.RemoveAll(projectDirectoryConfigV2)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectoryConfigV2, "--version", "2"},
		})
		editEndpointInConfig(filepath.Join(projectDirectoryConfigV2, defaultConfigFilename), hgeEndpoint)

		err := os.MkdirAll(filepath.Join(filepath.Join(projectDirectoryConfigV2, "seeds")), 0755)
		Expect(err).To(BeNil())
		file, err := os.Create(filepath.Join(projectDirectoryConfigV2, "seeds", "table_seed.sql"))
		Expect(err).To(BeNil())

		data := `INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`

		_, err = file.WriteString(data)
		Expect(err).To(BeNil())
		test(projectDirectoryConfigV2, nil, "postgres")
	})
})

var _ = Describe("seed apply (config v3)", func() {
	// automatic state migration should not affect new config v3 projects
	var projectDirectory string
	var teardown func()
	var pgSource = randomdata.SillyName()
	var citusSource = randomdata.SillyName()
	var mssqlSource = randomdata.SillyName()
	BeforeEach(func() {
		projectDirectory = testutil.RandDirName()
		hgeEndPort, teardownHGE := testutil.StartHasuraWithMetadataDatabase(GinkgoT(), testutil.HasuraDockerImage)
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%s", hgeEndPort)
		_, teardownPG := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, pgSource, "postgres")
		_, teardownCitus := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, citusSource, "citus")
		_, teardownMSSQL := testutil.AddDatabaseToHasura(GinkgoT(), hgeEndpoint, mssqlSource, "mssql")
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args: []string{"init", projectDirectory},
		})
		editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
		teardown = func() {
			os.RemoveAll(projectDirectory)
			teardownHGE()
			teardownPG()
			teardownCitus()
			teardownMSSQL()
		}
	})
	AfterEach(func() { teardown() })

	It("can apply seeds in config v3", func() {
		err := os.MkdirAll(filepath.Join(filepath.Join(projectDirectory, "seeds", pgSource)), 0755)
		Expect(err).To(BeNil())
		data := []byte(`INSERT INTO public.table1 (id) VALUES (1);
		INSERT INTO public.table1 (id) VALUES (2);
		INSERT INTO public.table1 (id) VALUES (3);
		INSERT INTO public.table1 (id) VALUES (4);`)

		mssqlData := []byte(`INSERT INTO "test"."table1" (id) VALUES (1);
		INSERT INTO "test"."table1" (id) VALUES (2);
		INSERT INTO "test"."table1" (id) VALUES (3);
		INSERT INTO "test"."table1" (id) VALUES (4);`)

		err = ioutil.WriteFile(filepath.Join(projectDirectory, "seeds", pgSource, "table_seed.sql"), data, 0655)
		Expect(err).To(BeNil())
		test(projectDirectory, []string{"--database-name", pgSource}, "postgres")

		err = os.MkdirAll(filepath.Join(filepath.Join(projectDirectory, "seeds", citusSource)), 0755)
		Expect(err).To(BeNil())
		err = ioutil.WriteFile(filepath.Join(projectDirectory, "seeds", citusSource, "table_seed.sql"), data, 0655)
		Expect(err).To(BeNil())
		test(projectDirectory, []string{"--database-name", citusSource}, "citus")

		err = os.MkdirAll(filepath.Join(filepath.Join(projectDirectory, "seeds", mssqlSource)), 0755)
		Expect(err).To(BeNil())
		err = ioutil.WriteFile(filepath.Join(projectDirectory, "seeds", mssqlSource, "table_seed.sql"), mssqlData, 0655)
		Expect(err).To(BeNil())
		test(projectDirectory, []string{"--database-name", mssqlSource}, "mssql")
	})

})
