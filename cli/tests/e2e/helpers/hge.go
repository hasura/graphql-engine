package helpers

import (
	"context"
	"fmt"
	"net"
	"os"
	"os/exec"
	"strings"

	"github.com/docker/go-connections/nat"

	"github.com/onsi/gomega"

	"github.com/google/uuid"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
	"github.com/testcontainers/testcontainers-go"
	tc "github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

func StartNewHge() (endpoint string, teardown func()) {
	// create a new postgres testcontainer
	if os.Getenv("CI") != "" {
		// start a postgres container
		ctx := context.Background()
		postgresC := startPostgresContainer(ctx)
		ip, err := postgresC.Host(ctx)
		Expect(err).ShouldNot(HaveOccurred())
		port, err := postgresC.MappedPort(ctx, "80")
		Expect(err).ShouldNot(HaveOccurred())
		postgresConnectionString := fmt.Sprintf("postgres://postgres:postgrespassword@%s:%d/postgres", ip, port.Int())

		// run HGE server on a random port
		hgePort := nat.Port(getFreePort())
		hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%d", hgePort.Int())
		session := startHgeBinary(postgresConnectionString, nat.Port(hgePort))

		teardown := func() {
			Expect(postgresC.Terminate(ctx)).ShouldNot(HaveOccurred())
			session.Terminate()
		}
		return hgeEndpoint, teardown
	}

	teardown = startHGEDockerCompose()
	return fmt.Sprintf("http://0.0.0.0:8080"), teardown
}

func startPostgresContainer(ctx context.Context) tc.Container {
	req := testcontainers.ContainerRequest{
		Image:        "postgres:12",
		ExposedPorts: []string{"5432/tcp"},
		Env: map[string]string{
			"POSTGRES_PASSWORD": "postgrespassword",
		},
		WaitingFor: wait.ForSQL(nat.Port(5432), "pq", func(port nat.Port) string {
			return fmt.Sprintf("postgres://postgres:postgrespassword@postgres:%d/postgres", port.Int())
		}),
	}
	postgresC, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	Expect(err).ShouldNot(HaveOccurred())
	return postgresC
}

func startHgeBinary(dbConnectionString string, port nat.Port) *Session {
	var hgeArgs = []string{
		"--database-url",
		dbConnectionString,
		"serve",
		"--server-port",
		port.Port(),
		"--enable-console --console-assets-dir=../console/static/dist",
	}
	cmd := exec.Command("/build/_server_output/graphql-engine", hgeArgs...)
	session, err := Start(
		cmd,
		NewPrefixedWriter(DebugOutPrefix, GinkgoWriter),
		NewPrefixedWriter(DebugErrPrefix, GinkgoWriter),
	)
	Expect(err).ShouldNot(HaveOccurred())
	return session
}

func startHGEDockerCompose() func() {
	composeFilePaths := []string{"testdata/docker-compose-tmpl.yaml"}
	identifier := strings.ToLower(uuid.New().String())
	compose := tc.NewLocalDockerCompose(composeFilePaths, identifier)
	execError := compose.WithCommand([]string{"up", "-d"}).Invoke()
	err := execError.Error
	Expect(err).ShouldNot(HaveOccurred())

	teardown := func() {
		execError := compose.WithCommand([]string{"down", "-v"}).Invoke()
		err := execError.Error
		if err != nil {
			Expect(err).ShouldNot(gomega.HaveOccurred())
		}
	}
	return teardown
}

func getFreePort() int {
	addr, err := net.ResolveTCPAddr("tcp", "localhost:0")
	Expect(err).ShouldNot(HaveOccurred())

	l, err := net.ListenTCP("tcp", addr)
	Expect(err).ShouldNot(HaveOccurred())
	defer l.Close()
	return l.Addr().(*net.TCPAddr).Port
}
