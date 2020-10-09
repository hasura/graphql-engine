package helpers

import (
	"context"
	"fmt"
	"net"
	"os/exec"
	"strconv"
	"strings"

	"github.com/docker/go-connections/nat"

	"github.com/onsi/gomega"

	_ "database/sql"

	"github.com/google/uuid"
	"github.com/hashicorp/go-retryablehttp"
	_ "github.com/lib/pq"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	. "github.com/onsi/gomega/gexec"
	"github.com/testcontainers/testcontainers-go"
	tc "github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

func StartNewHge() (endpoint string, teardown func()) {
	switch {
	case IsHGEBinaryPathSet():
		{
			fmt.Fprintln(GinkgoWriter, hgeBinaryPath, hgeServerDirectoryPath)
			// start a postgres container
			ctx := context.Background()
			postgresC := startPostgresContainer(ctx)
			ip, err := postgresC.Host(ctx)
			Expect(err).ShouldNot(HaveOccurred())
			port, err := postgresC.MappedPort(ctx, "5432")
			Expect(err).ShouldNot(HaveOccurred())
			postgresConnectionString := fmt.Sprintf("postgres://postgres:postgrespassword@%s:%d/postgres", ip, port.Int())

			// run HGE server on a random port
			if hgeBinaryPath == "" {
				hgeBinaryPath = "/build/_server_output/graphql-engine"
			}
			hgePort := getFreePort()
			hgeEndpoint := fmt.Sprintf("http://0.0.0.0:%d", hgePort)
			session := startHgeBinary(startHgeBinaryOpts{
				postgresConnectionString, hgePort, hgeBinaryPath, hgeServerDirectoryPath,
			})

			teardown := func() {
				Expect(postgresC.Terminate(ctx)).ShouldNot(HaveOccurred())
				session.Terminate()
			}
			return hgeEndpoint, teardown
		}
	default:
		teardown = startHGEDockerCompose()
		return "http://0.0.0.0:8080", teardown
	}
}

func startPostgresContainer(ctx context.Context) tc.Container {
	port, err := nat.NewPort("tcp", "5432")
	Expect(err).ShouldNot(HaveOccurred())
	req := testcontainers.ContainerRequest{
		Image:        "postgres:12",
		ExposedPorts: []string{"5432/tcp"},
		Env: map[string]string{
			"POSTGRES_PASSWORD": "postgrespassword",
			"POSTGRES_USER":     "postgres",
			"POSTGRES_DB":       "hge-test",
		},
		WaitingFor: wait.ForSQL(port, "postgres", func(port nat.Port) string {
			return fmt.Sprintf("postgres://postgres:postgrespassword@localhost:%s/hge-test?sslmode=disable", port.Port())
		}),
	}
	postgresC, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	Expect(err).ShouldNot(HaveOccurred())
	return postgresC
}

type startHgeBinaryOpts struct {
	DbConnectionString string
	Port               int
	BinaryPath         string
	WorkingDirectory   string
}

func startHgeBinary(opts startHgeBinaryOpts) *Session {
	var hgeArgs = []string{
		"--database-url",
		opts.DbConnectionString,
		"serve",
		"--server-port",
		strconv.Itoa(opts.Port),
		"--server-host",
		"0.0.0.0",
		"--enable-console",
		"--console-assets-dir=../console/static/dist",
	}
	if v := strings.Split(opts.BinaryPath, " "); len(v) > 1 {
		// for handling paths like
		// cabal new-run -- exe:graphql-engine
		opts.BinaryPath = v[0]
		hgeArgs = append(v[1:], hgeArgs...)
	}
	cmd := exec.Command(opts.BinaryPath, hgeArgs...)
	if opts.WorkingDirectory != "" {
		cmd.Dir = opts.WorkingDirectory
	}
	session, err := Start(
		cmd,
		NewPrefixedWriter(DebugOutPrefix, GinkgoWriter),
		NewPrefixedWriter(DebugErrPrefix, GinkgoWriter),
	)
	fmt.Fprint(GinkgoWriter, "starting command", opts.BinaryPath, hgeArgs)
	Expect(err).ShouldNot(HaveOccurred())
	// wait for server to start
	retryClient := retryablehttp.NewClient()
	retryClient.RetryMax = 10
	_, err = retryablehttp.Get(fmt.Sprintf("%s:%d/healthz", "http://0.0.0.0", opts.Port))
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

	// wait for server to be ready
	retryClient := retryablehttp.NewClient()
	retryClient.RetryMax = 10
	_, err = retryablehttp.Get(fmt.Sprintf("%s:%d/healthz", "http://0.0.0.0", 8080))
	Expect(err).ShouldNot(HaveOccurred())

	teardown := func() {
		execError := compose.Down()
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
