package acceptance

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("init command", func() {
	When("given a directory as argument", func() {
		It("should create project directory successfully", func() {
			session := Hasura("init", "test")
			want := `.*directory created\. execute the following commands to continue:.*`
			Eventually(session, 5).Should(Say(want))
			Eventually(session, 5).Should(Exit(0))
		}, 5)
	})
	//
})

func TestInitCommand(t *testing.T) {

}
