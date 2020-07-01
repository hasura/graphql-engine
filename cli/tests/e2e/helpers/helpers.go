package helpers

import (
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"

	"github.com/sirupsen/logrus"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
)

const (
	DebugCommandPrefix        = "\nCMD>"
	DebugCommandPrefixWithDir = "\nCMD %s>"
	DebugOutPrefix            = "OUT: "
	DebugErrPrefix            = "ERR: "
)

func Hasura(args ...string) *Session {
	hasuraBinaryPath := "hasura"
	if os.Getenv("CI") != "" {
		hasuraBinaryPath = "/build/_cli_output/binaries/cli-hasura-linux-amd64"
	}
	cmd := exec.Command(hasuraBinaryPath, args...)
	session, err := Start(
		cmd,
		NewPrefixedWriter(DebugOutPrefix, GinkgoWriter),
		NewPrefixedWriter(DebugErrPrefix, GinkgoWriter),
	)
	Expect(err).NotTo(HaveOccurred())
	return session
}
func RunCommandAndSucceed(args ...string) *Session {
	session := Hasura(args...)
	Eventually(session, 5).Should(Exit(0))
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
