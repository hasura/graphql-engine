package e2e

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/tests/e2e/helpers"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("metadata_command", func() {

	var dirName string
	var session *Session
	var teardown func()
	BeforeEach(func() {
		dirName = helpers.RandDirName()
		fmt.Println("dir:", dirName)
		hgeEndpoint, teardownHGE := helpers.StartNewHge()
		helpers.RunCommandAndSucceed(helpers.CmdOpts{
			Args: []string{"init", dirName},
		})
		helpers.EditEndpointInConfig(filepath.Join(dirName, defaultConfigFilename), hgeEndpoint)

		teardown = func() {
			session.Kill()
			os.RemoveAll(dirName)
			teardownHGE()
		}
	})

	AfterEach(func() {
		teardown()
	})

	Context("metadata diff", func() {
		It("should output the metadata of server and local directory", func() {
			session = helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "diff"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				".*tables*.",
				".*cron_triggers*.",
			}

			for _, keyword := range wantKeywordList {
				Eventually(session, 60*40).Should(Say(keyword))
			}
			Eventually(session, 60*40).Should(Exit(0))
		})
	})

	Context("metadata apply", func() {
		It("should apply metadata to server", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "apply"},
				WorkingDirectory: dirName,
			})
			want := `.*Metadata applied*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata clear", func() {
		It("should clear metadata on server", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "clear"},
				WorkingDirectory: dirName,
			})
			want := `.*Metadata cleared*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata export", func() {
		It("should export metadata from server", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "export"},
				WorkingDirectory: dirName,
			})
			want := `.*Metadata exported*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata reload", func() {
		It("should reload metadata", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "reload"},
				WorkingDirectory: dirName,
			})
			want := `.*Metadata reloaded*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata inconsistency", func() {
		It("Manage inconsistent objects in Hasura metadata", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "inconsistency"},
				WorkingDirectory: dirName,
			})
			want := `.*Manage inconsistent objects in Hasura Metadata*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata inconsistency list", func() {
		It("Lists all inconsistent objects from the metadata", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "list"},
				WorkingDirectory: dirName,
			})
			want := `.*metadata is consistent*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata inconsistency drop", func() {
		It("Drops inconsistent objects from the metadata", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "drop"},
				WorkingDirectory: dirName,
			})
			want := `.*all inconsistent objects removed from metadata*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})

	Context("metadata inconsistency status", func() {
		It("Checks if the metadata is inconsistent or not", func() {
			session := helpers.Hasura(helpers.CmdOpts{
				Args:             []string{"metadata", "inconsistency", "status"},
				WorkingDirectory: dirName,
			})
			want := `.*metadata is consistent*.`
			Eventually(session, 60).Should(Say(want))
			Eventually(session, 60).Should(Exit(0))
		})
	})
})
