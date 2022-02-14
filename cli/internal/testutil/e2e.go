package testutil

import (
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/mitchellh/go-homedir"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
	"github.com/sirupsen/logrus"
)

const (
	DebugCommandPrefix        = "\nCMD>"
	DebugCommandPrefixWithDir = "\nCMD %s>"
	DebugOutPrefix            = "OUT: "
	DebugErrPrefix            = "ERR: "
	DefaultE2ETestTimeout     = 60 // in seconds
)

type CmdOpts struct {
	Args             []string
	WorkingDirectory string
}

func Hasura(opts CmdOpts) *Session {
	var hasuraBinaryPath = CLIBinaryPath
	args := append([]string{"--skip-update-check"}, opts.Args...)
	cmd := exec.Command(hasuraBinaryPath, args...)
	if opts.WorkingDirectory != "" {
		cmd.Dir = opts.WorkingDirectory
	}
	session, err := Start(
		cmd,
		NewPrefixedWriter(DebugOutPrefix, GinkgoWriter),
		NewPrefixedWriter(DebugErrPrefix, GinkgoWriter),
	)
	Expect(err).NotTo(HaveOccurred())
	return session
}
func RunCommandAndSucceed(opts CmdOpts) *Session {
	session := Hasura(opts)
	Eventually(session, DefaultE2ETestTimeout).Should(Exit(0))
	return session
}

func RandDirName() string {
	file, err := ioutil.TempFile("", "cli-e2e-*")
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		file.Close()
		defer os.Remove(file.Name())
	}()
	return file.Name()
}

func RemoveDir(dirName string) {
	err := os.RemoveAll(dirName)
	if err != nil {
		log.Println(err)
	}
}

func CloseWithLogOnErr(closer io.Closer) {
	err := closer.Close()
	if err == nil {
		return
	}

	logger := logrus.New()
	logger.Out = GinkgoWriter
	logger.Error(err)
}

func RemoveHasuraConfigHomeDirectory() {
	homeDir, err := homedir.Dir()
	Expect(err).ShouldNot(HaveOccurred())
	err = os.RemoveAll(filepath.Join(homeDir, ".hasura"))
	Expect(err).ShouldNot(HaveOccurred())
}

// to run commands other than hasura
func Command(cmdPath string, opts CmdOpts) *Session {
	cmd := exec.Command(cmdPath, opts.Args...)
	if opts.WorkingDirectory != "" {
		cmd.Dir = opts.WorkingDirectory
	}
	session, err := Start(
		cmd,
		NewPrefixedWriter(DebugOutPrefix, GinkgoWriter),
		NewPrefixedWriter(DebugErrPrefix, GinkgoWriter),
	)
	Expect(err).NotTo(HaveOccurred())
	return session
}
