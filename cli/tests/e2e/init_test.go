package e2e

import (
	"fmt"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/tests/e2e/helpers"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("init command", func() {
	When("given a directory as argument", func() {
		It("should create a V2 project directory successfully", func() {
			dirName := helpers.RandDirName()
			defer helpers.RemoveDir(dirName)

			session := helpers.Hasura("init", dirName)
			want := fmt.Sprintf(`.*directory created\. execute the following commands to continue:.*cd %s`, dirName)
			Eventually(session, 5).Should(Say(want))

			Eventually(session, 5).Should(Exit(0))

			wantDirList := []string{
				"migrations",
				"seeds",
				"metadata",
			}
			wantFileList := []string{
				"config.yaml",
			}
			for _, d := range wantDirList {
				Expect(filepath.Join(dirName, d)).Should(BeADirectory())
			}
			for _, d := range wantFileList {
				Expect(filepath.Join(dirName, d)).Should(BeAnExistingFile())
			}
		}, 5)

		When("--version 1 flag is given", func() {
			It("should create a v1 project directory", func() {
				dirName := helpers.RandDirName()
				defer helpers.RemoveDir(dirName)

				session := helpers.Hasura("init", dirName)
				want := `.*directory created\. execute the following commands to continue:.*`
				Eventually(session, 5).Should(Say(want))
				Eventually(session, 5).Should(Exit(0))

				wantDirList := []string{
					"migrations",
					"seeds",
				}
				wantFileList := []string{
					"config.yaml",
				}
				for _, d := range wantDirList {
					Expect(filepath.Join(dirName, d)).Should(BeADirectory())
				}
				for _, d := range wantFileList {
					Expect(filepath.Join(dirName, d)).Should(BeAnExistingFile())
				}
			})
		})
	})
})
