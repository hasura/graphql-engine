package e2e

import (
	"io/ioutil"
	"net/http"
	"os"

	"github.com/hasura/graphql-engine/cli/tests/e2e/helpers"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("console command", func() {
	var teardown func()
	BeforeSuite(func() {
		projectDir := helpers.RandDirName()
		helpers.RunCommandAndSucceed("init", projectDir)
		err := os.Chdir(projectDir)
		Expect(err).To(BeNil())

		teardown = func() {
			os.RemoveAll(projectDir)
		}
	})
	AfterSuite(func() {
		teardown()
	})

	When("running console command without any args", func() {
		It("should open console on port 9695, api port on 9693", func() {
			session := helpers.Hasura("console")

			resp, err := http.Get("http://localhost:9695/console")
			Expect(err).To(BeNil())
			Expect(resp.StatusCode).To(Equal(http.StatusOK))
			resp, err = http.Get("http://localhost:9693/apis/migrate")
			Expect(err).To(BeNil())
			Expect(resp.StatusCode).To(Equal(http.StatusOK))
			b, err := ioutil.ReadAll(resp.Body)
			Expect(b).Should(MatchJSON(`{"migrations":[],"status":{}}`))
			defer resp.Body.Close()

			session.Signal(os.Interrupt)
			Eventually(session, 5).Should(Exit())
		})
	})
})
