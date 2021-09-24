package commands

import (
	"fmt"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/hasura/graphql-engine/cli/v2/util"
	. "github.com/onsi/ginkgo"
	"io/ioutil"
	"path/filepath"

	. "github.com/onsi/gomega"
	//. "github.com/onsi/gomega/gbytes"
	//. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura scripts update-project-v2", func() {
	var teardown func()
	var hgeEndpoint string
	BeforeEach(func() {
		// start cli-migrations (config v1)
		connectionUrl, teardownPG := testutil.StartPGContainer(GinkgoT())
		_, teardownV13 := testutil.StartHasuraCLIMigrations(
			GinkgoT(),
			"hasura/graphql-engine:v1.3.3.cli-migrations",
			connectionUrl,
			"",
			func() string {
				s, err := filepath.Abs("testdata/config-v1-test-project/migrations")
				Expect(err).To(BeNil())
				return s
			}(),
		)
		teardownV13()

		port, teardownLatest := testutil.StartHasuraWithPG(GinkgoT(), testutil.HasuraDockerImage, connectionUrl)
		hgeEndpoint = fmt.Sprintf("http://%s:%s", testutil.Hostname, port)
		teardown = func() {
			teardownPG()
			teardownLatest()
		}
	})
	AfterEach(func() { teardown() })

	It("can update a config v1 project to config v2", func() {
		tmpDir, err := ioutil.TempDir("", "hasura-cli-test-*")
		Expect(err).To(BeNil())
		projectDir := filepath.Join(tmpDir, "project")
		Expect(util.CopyDir("testdata/config-v1-test-project", projectDir)).To(BeNil())
		editEndpointInConfig(filepath.Join(projectDir, defaultConfigFilename), hgeEndpoint)
		testutil.RunCommandAndSucceed(testutil.CmdOpts{
			Args:             []string{"scripts", "update-project-v2"},
			WorkingDirectory: projectDir,
		})
		Expect(filepath.Join(projectDir, "metadata", "tables.yaml")).To(BeAnExistingFile())
		Expect(filepath.Join(projectDir, "migrations", "1626431381198_create_table_public_t1", "up.sql")).To(BeAnExistingFile())
	})
})
