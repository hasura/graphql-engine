package e2e

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"os"

	"github.com/hasura/graphql-engine/cli/tests/e2e/helpers"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
)

var _ = Describe("console command", func() {
	Context("v1 project", func() {
		var teardown func()
		projectDir := helpers.RandDirName()
		BeforeEach(func() {
			// TODO: run from a fresh db
			helpers.RunCommandAndSucceed(helpers.CmdOpts{
				Args: []string{"init", projectDir, "--version", "1"},
			})
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"console", "--no-browser"},
				WorkingDirectory: projectDir,
			})

			want := `.*console running at: http://localhost:9695/.*`
			Eventually(session, 5).Should(Say(want))

			teardown = func() {
				session.Signal(os.Interrupt)
				os.RemoveAll(projectDir)
				os.Chdir(os.TempDir())
			}
		})
		AfterEach(func() {
			teardown()
		})

		When("api requests are send", func() {
			It("generates metadata and migration files", func() {
				checkRequiredServersAreStarted()
				sendAPIRequestsToCreateMetadataAndMigrations()
			})
		})
	})
	Context("v2 project", func() {
		var teardown func()
		projectDir := helpers.RandDirName()
		BeforeEach(func() {
			fmt.Fprintln(GinkgoWriter, "creating this", projectDir)
			helpers.RunCommandAndSucceed(helpers.CmdOpts{
				Args: []string{"init", projectDir},
			})
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"console", "--no-browser"},
				WorkingDirectory: projectDir,
			})
			want := `.*console running at: http://localhost:9695/.*`
			Eventually(session, 5).Should(Say(want))

			teardown = func() {
				session.Terminate()
				os.RemoveAll(projectDir)
			}
		})
		AfterEach(func() {
			teardown()
		})
		When("api requests are send", func() {
			It("generates metadata and migration files", func() {
				checkRequiredServersAreStarted()
				sendAPIRequestsToCreateMetadataAndMigrations()
			})
		})
	})
})

func checkRequiredServersAreStarted() {
	resp, err := http.Get("http://localhost:9695/console")
	Expect(err).ShouldNot(HaveOccurred())
	Expect(resp.StatusCode).To(Equal(http.StatusOK))
	resp, err = http.Get("http://localhost:9693/apis/migrate")
	Expect(err).ShouldNot(HaveOccurred())
	Expect(resp.StatusCode).To(Equal(http.StatusOK))
	b, err := ioutil.ReadAll(resp.Body)
	Expect(err).ShouldNot(HaveOccurred())
	Expect(b).Should(MatchJSON(`{"migrations":[],"status":{}}`))
	defer resp.Body.Close()
}

func sendAPIRequestsToCreateMetadataAndMigrations() {
	resp, err := helpers.SendToHasuraMigrateAPIBodyFromFile("testdata/fixtures/create_table_users.json")
	Expect(err).ShouldNot(HaveOccurred())
	Expect(resp.StatusCode).To(Equal(http.StatusOK))
}
