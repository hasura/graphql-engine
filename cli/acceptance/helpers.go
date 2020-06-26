package acceptance

import (
	"os/exec"

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
	cmd := exec.Command("hasura", args...)
	session, err := Start(
		cmd,
		NewPrefixedWriter(DebugOutPrefix, GinkgoWriter),
		NewPrefixedWriter(DebugErrPrefix, GinkgoWriter),
	)
	Expect(err).NotTo(HaveOccurred())
	return session
}
