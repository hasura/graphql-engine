package e2e

import (
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/tests/e2e/helpers"
	"github.com/mitchellh/go-homedir"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("plugins command", func() {
	Describe("install plugin", func() {
		BeforeEach(func() {
			helpers.RemoveHasuraConfigHomeDirectory()
		})
		When("when plugin name is given as argument", func() {
			It("can install plugin successfully", func() {
				session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "install", "cli-ext"}})
				want := `.*plugin installed.*`
				Eventually(session, 60*3).Should(Say(want))
				Eventually(session, 60*3).Should(Exit(0))
			})
			When("plugin version to be installed is given via the --version flag", func() {
				It("can install the correct version", func() {
					session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "install", "cli-ext", "--version", "v1.3.0-beta.2"}})
					want := `.*plugin installed.*`
					Eventually(session, 60*3).Should(Say(want))
					Eventually(session, 60*3).Should(Exit(0))

					session = helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "list"}})
					want = `.*Hasura CLI extension.*v1.3.0-beta.2.*`
					Eventually(session, 60*3).Should(Say(want))
					Eventually(session, 60*3).Should(Exit(0))
				})
			})
		})
	})
	Describe("list plugin", func() {
		BeforeEach(func() {
			helpers.RemoveHasuraConfigHomeDirectory()
		})
		It("before a plugin is installed the installed status is no", func() {
			session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "list"}})
			want := `.*Hasura CLI extension.*no.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))
		})
		It("after a plugin is installed the installed status should change to yes", func() {
			session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "install", "cli-ext"}})
			want := `.*plugin installed.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))

			session = helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "list"}})
			want = `.*Hasura CLI extension.*yes.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))
		})

	})
	Describe("uninstall plugin", func() {
		BeforeEach(func() {
			helpers.RemoveHasuraConfigHomeDirectory()
		})
		It("throws an error when we try to uninstall a plugin which is not installed", func() {
			session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "uninstall", "nobodynametheirpluginthis"}})
			want := `.*failed to uninstall.*`
			Eventually(session.Err, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(1))
		})
		It("can uninstall an installed plugin", func() {
			session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "install", "cli-ext"}})
			want := `.*plugin installed.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))

			session = helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "uninstall", "cli-ext"}})
			want = `.*plugin uninstalled.*cli-ext.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))

			// make sure the binary was also removed
			homeDir, err := homedir.Dir()
			Expect(err).To(BeNil())
			Expect(filepath.Join(homeDir, ".hasura/bin/cli-ext-hasura-linux")).ShouldNot(BeAnExistingFile())
		})
	})
	Describe("upgrade plugin", func() {
		BeforeEach(func() {
			helpers.RemoveHasuraConfigHomeDirectory()
		})
		It("can successfully upgrade from a previous version of plugin", func() {
			session := helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "install", "cli-ext", "--version", "v1.2.0"}})
			want := `.*plugin installed.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))

			session = helpers.Hasura(helpers.CmdOpts{Args: []string{"plugins", "upgrade", "cli-ext"}})
			want = `.*Plugin upgraded.*`
			Eventually(session, 60*3).Should(Say(want))
			Eventually(session, 60*3).Should(Exit(0))
		})
	})

})
