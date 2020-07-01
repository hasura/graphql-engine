package helpers

import (
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"

	"gopkg.in/yaml.v2"

	"github.com/hasura/graphql-engine/cli"
	"github.com/mitchellh/go-homedir"

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

type CmdOpts struct {
	Args             []string
	WorkingDirectory string
}

func Hasura(opts CmdOpts) *Session {
	hasuraBinaryPath := "hasura"
	if os.Getenv("CI") != "" {
		hasuraBinaryPath = "/build/_cli_output/binaries/cli-hasura-linux-amd64"
	}
	cmd := exec.Command(hasuraBinaryPath, opts.Args...)
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

func RemoveHasuraConfigHomeDirectory() {
	homeDir, err := homedir.Dir()
	Expect(err).To(BeNil())
	err = os.RemoveAll(filepath.Join(homeDir, ".hasura"))
	Expect(err).To(BeNil())
}

// EditEndpoint in config
func EditEndpointInConfig(configFilePath, endpoint string) error {
	var config cli.Config
	b, err := ioutil.ReadFile(configFilePath)
	if err != nil {
		return err
	}
	err = yaml.Unmarshal(b, config)
	if err != nil {
		return err
	}
	config.Endpoint = endpoint

	b, err = yaml.Marshal(config)
	if err != nil {
		return err
	}

	err = ioutil.WriteFile(configFilePath, b, 0655)
	if err != nil {
		return err
	}
	return nil
}
