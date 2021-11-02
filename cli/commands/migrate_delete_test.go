package commands

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gbytes"
	. "github.com/onsi/gomega/gexec"
)

var _ = Describe("hasura migrate delete", func() {
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

	Context("migrate delete --all", func() {
		It("should delete the migrations on server and on source ", func() {
			projectDirectory := testutil.RandDirName()
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectory},
			})
			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: projectDirectory,
			})

			str := string(session.Err.Contents())
			i := strings.Index(str, "\"version\"")
			version := str[i+10 : i+23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default"},
				WorkingDirectory: projectDirectory,
			})
			wantKeywordList := []string{
				"migrations applied",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Err).Should(Say(keyword))
			}

			args := []string{"migrate", "delete", "--all", "--database-name", "default", "--force"}
			cmd := exec.Command(testutil.CLIBinaryPath, args...)
			cmd.Dir = projectDirectory
			session, err := Start(
				cmd,
				NewPrefixedWriter(testutil.DebugOutPrefix, GinkgoWriter),
				NewPrefixedWriter(testutil.DebugErrPrefix, GinkgoWriter),
			)
			Expect(err).To(BeNil())
			Expect(session.Wait(timeout).Err).Should(Say("Deleted migrations"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status", "--database-name", "default"},
				WorkingDirectory: projectDirectory,
			})
			Expect(session.Wait(timeout).Err).ShouldNot(Say(version))
			Eventually(session, timeout).Should(Exit(0))
		})
	})

	Context("migrate delete --all --server", func() {
		It("should delete the migrations on server and on source ", func() {
			projectDirectory := testutil.RandDirName()
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectory},
			})
			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: projectDirectory,
			})

			str := string(session.Err.Contents())
			i := strings.Index(str, "\"version\"")
			version := str[i+10 : i+23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default"},
				WorkingDirectory: projectDirectory,
			})
			wantKeywordList := []string{
				"migrations applied",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Err).Should(Say(keyword))
			}

			args := []string{"migrate", "delete", "--all", "--server", "--database-name", "default", "--force"}
			cmd := exec.Command(testutil.CLIBinaryPath, args...)
			cmd.Dir = projectDirectory
			session, err := Start(
				cmd,
				NewPrefixedWriter(testutil.DebugOutPrefix, GinkgoWriter),
				NewPrefixedWriter(testutil.DebugErrPrefix, GinkgoWriter),
			)
			Expect(err).To(BeNil())
			Expect(session.Wait(timeout).Err).Should(Say("Deleted migrations"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status", "--database-name", "default"},
				WorkingDirectory: projectDirectory,
			})
			Eventually(session.Out).Should(Say(version))
			Eventually(session.Out).Should(Say("Present"))
			Eventually(session.Out).Should(Say("Not Present"))
			Eventually(session, timeout).Should(Exit(0))
		})
	})

	Context("migrate delete --version <version>", func() {
		It("should delete the migrations on server and on source ", func() {
			dirName := testutil.RandDirName()
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", dirName},
			})

			editEndpointInConfig(filepath.Join(dirName, defaultConfigFilename), hgeEndpoint)
			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;", "--database-name", "default"},
				WorkingDirectory: dirName,
			})

			str := string(session.Err.Contents())
			i := strings.Index(str, "\"version\"")
			version := str[i+10 : i+23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			wantKeywordList := []string{
				"migrations applied",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Err).Should(Say(keyword))
			}

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "delete", "--version", version, "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			Expect(session.Wait(timeout).Err).Should(Say("Deleted migrations"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status", "--database-name", "default"},
				WorkingDirectory: dirName,
			})
			Expect(session.Wait(timeout).Err).ShouldNot(Say(version))
			Eventually(session, timeout).Should(Exit(0))
		})
	})

	Context("migrate delete --version <version> (config v2)", func() {
		It("should delete the migrations on server and on source ", func() {
			projectDirectory := testutil.RandDirName()
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--version", "2"},
			})

			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)
			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;"},
				WorkingDirectory: projectDirectory,
			})

			str := string(session.Err.Contents())
			i := strings.Index(str, "\"version\"")
			version := str[i+10 : i+23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply"},
				WorkingDirectory: projectDirectory,
			})
			wantKeywordList := []string{
				"migrations applied",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Err).Should(Say(keyword))
			}

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "delete", "--version", version},
				WorkingDirectory: projectDirectory,
			})
			Expect(session.Wait(timeout).Err).Should(Say("Deleted migrations"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status"},
				WorkingDirectory: projectDirectory,
			})
			Expect(session.Wait(timeout).Err).ShouldNot(Say(version))
			Eventually(session, timeout).Should(Exit(0))
		})
	})

	Context("migrate delete --all (config v2)", func() {
		It("should delete the migrations on server and on source ", func() {
			projectDirectory := testutil.RandDirName()
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--version", "2"},
			})
			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;"},
				WorkingDirectory: projectDirectory,
			})

			str := string(session.Err.Contents())
			i := strings.Index(str, "\"version\"")
			version := str[i+10 : i+23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply"},
				WorkingDirectory: projectDirectory,
			})
			wantKeywordList := []string{
				"migrations applied",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Err).Should(Say(keyword))
			}

			args := []string{"migrate", "delete", "--all", "--force"}
			cmd := exec.Command(testutil.CLIBinaryPath, args...)
			cmd.Dir = projectDirectory
			session, err := Start(
				cmd,
				NewPrefixedWriter(testutil.DebugOutPrefix, GinkgoWriter),
				NewPrefixedWriter(testutil.DebugErrPrefix, GinkgoWriter),
			)
			Expect(err).To(BeNil())
			Expect(session.Wait(timeout).Err).Should(Say("Deleted migrations"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status"},
				WorkingDirectory: projectDirectory,
			})
			Expect(session.Wait(timeout).Err).ShouldNot(Say(version))
			Eventually(session, timeout).Should(Exit(0))
		})
	})

	Context("migrate delete --all --server(config v2)", func() {
		It("should delete the migrations on server and on source ", func() {
			projectDirectory := testutil.RandDirName()
			testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args: []string{"init", projectDirectory, "--version", "2"},
			})
			editEndpointInConfig(filepath.Join(projectDirectory, defaultConfigFilename), hgeEndpoint)

			session := testutil.RunCommandAndSucceed(testutil.CmdOpts{
				Args:             []string{"migrate", "create", "schema_creation", "--up-sql", "create schema \"testing\";", "--down-sql", "drop schema \"testing\" cascade;"},
				WorkingDirectory: projectDirectory,
			})

			str := string(session.Err.Contents())
			i := strings.Index(str, "\"version\"")
			version := str[i+10 : i+23]
			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "apply"},
				WorkingDirectory: projectDirectory,
			})
			wantKeywordList := []string{
				"migrations applied",
			}

			for _, keyword := range wantKeywordList {
				Expect(session.Wait(timeout).Err).Should(Say(keyword))
			}

			args := []string{"migrate", "delete", "--all", "--server", "--force"}
			cmd := exec.Command(testutil.CLIBinaryPath, args...)
			cmd.Dir = projectDirectory
			session, err := Start(
				cmd,
				NewPrefixedWriter(testutil.DebugOutPrefix, GinkgoWriter),
				NewPrefixedWriter(testutil.DebugErrPrefix, GinkgoWriter),
			)
			Expect(err).To(BeNil())
			Expect(session.Wait(timeout).Err).Should(Say("Deleted migrations"))

			session = testutil.Hasura(testutil.CmdOpts{
				Args:             []string{"migrate", "status"},
				WorkingDirectory: projectDirectory,
			})
			Expect(session.Wait(timeout).Out).Should(Say(version))
			Expect(session.Wait(timeout).Out).Should(Say("Present"))
			Expect(session.Wait(timeout).Out).Should(Say("Not Present"))
			Eventually(session, timeout).Should(Exit(0))
		})
	})
})
