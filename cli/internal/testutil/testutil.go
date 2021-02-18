package testutil

import (
	"database/sql"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
	_ "github.com/lib/pq"
	"github.com/ory/dockertest/v3"
)

func StartHasura(t *testing.T, version string) (port string, teardown func()) {
	t.Skip()
	if len(version) == 0 {
		t.Fatal("no hasura version provided, probably use testutil.HasuraVersion")
	}
	var err error
	pool, err := dockertest.NewPool("")
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	testDirectory, err := ioutil.TempDir(os.TempDir(), "hasura-cli-test-*")
	if err != nil {
		t.Fatal(err)
	}
	testName := filepath.Base(testDirectory)
	pgopts := &dockertest.RunOptions{
		Name:       fmt.Sprintf("%s-%s", testName, "pg"),
		Repository: "postgres",
		Tag:        "11",
		Env: []string{
			"POSTGRES_PASSWORD=postgrespassword",
			"POSTGRES_DB=postgres",
		},
		ExposedPorts: []string{"5432/tcp"},
	}
	pg, err := pool.RunWithOptions(pgopts)
	if err != nil {
		t.Fatalf("Could not start resource: %s", err)
	}
	var db *sql.DB
	if err = pool.Retry(func() error {
		var err error
		db, err = sql.Open("postgres", fmt.Sprintf("postgres://postgres:postgrespassword@%s:%s/%s?sslmode=disable", DockerSwitchIP, pg.GetPort("5432/tcp"), "postgres"))
		if err != nil {
			return err
		}
		return db.Ping()
	}); err != nil {
		t.Fatal(err)
	}
	hasuraopts := &dockertest.RunOptions{
		Name:       fmt.Sprintf("%s-%s", testName, "hasura"),
		Repository: "hasura/graphql-engine",
		Tag:        version,
		Env: []string{
			fmt.Sprintf("HASURA_GRAPHQL_DATABASE_URL=postgres://postgres:postgrespassword@%s:%s/postgres", DockerSwitchIP, pg.GetPort("5432/tcp")),
			`HASURA_GRAPHQL_ENABLE_CONSOLE=true`,
			"HASURA_GRAPHQL_DEV_MODE=true",
			"HASURA_GRAPHQL_ENABLED_LOG_TYPES=startup, http-log, webhook-log, websocket-log, query-log",
		},
		ExposedPorts: []string{"8080/tcp"},
	}
	hasura, err := pool.RunWithOptions(hasuraopts)
	if err != nil {
		t.Fatalf("Could not start resource: %s", err)
	}

	if err = pool.Retry(func() error {
		var err error
		resp, err := http.Get(fmt.Sprintf("http://localhost:%s/healthz", hasura.GetPort("8080/tcp")))
		if err != nil {
			return err
		}
		if resp.StatusCode != http.StatusOK {
			return errors.New("not ready")
		}
		return nil
	}); err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}

	teardown = func() {
		if err = pool.Purge(hasura); err != nil {
			t.Fatalf("Could not purge resource: %s", err)
		}
		if err = pool.Purge(pg); err != nil {
			t.Fatalf("Could not purge resource: %s", err)
		}
	}
	return hasura.GetPort("8080/tcp"), teardown
}

func NewHttpcClient(t *testing.T, port string, headers map[string]string) *httpc.Client {
	c, err := httpc.New(nil, fmt.Sprintf("%s:%s/", BaseURL, port), headers)
	if err != nil {
		t.Fatal(err)
	}
	return c
}
