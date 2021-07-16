package testutil

import (
  "context"
  "database/sql"
  "errors"
  "fmt"
  "github.com/ory/dockertest/v3/docker"
  "io/ioutil"
  "net/http"
  "os"
  "strings"
  "testing"
  "time"

  "github.com/gofrs/uuid"
  "github.com/stretchr/testify/require"

  "github.com/Pallinder/go-randomdata"

  _ "github.com/denisenkom/go-mssqldb"
  "github.com/hasura/graphql-engine/cli/v2/internal/httpc"
  _ "github.com/lib/pq"
  "github.com/ory/dockertest/v3"
)

// helper function to get image repo and tag separately
func getDockerRepoAndTag(t TestingT, dockerImage string) (repo, version string) {
	p := strings.Split(dockerImage, ":")
	if len(p) == 2 {
		return p[0], p[1]
	} else {
		t.Fatalf("expected to find a docker image with repo and version (repo:tag) found: %v", p)
	}
	return "", ""
}

// TestingT is a workaround for using test helpers on Ginkgo tests
// and normal go tests this interfaces is introduced
// ginkgo specs do not have a handle of *testing.T and therefore
// cannot be used directly in test helpers
type TestingT interface {
	Skip(args ...interface{})
	Fatal(args ...interface{})
	Fatalf(format string, args ...interface{})
}

func StartHasura(t TestingT, image string) (port string, teardown func()) {
	connectionUrl, teardownPG := StartPGContainer(t)
	port, teardownHasura := StartHasuraWithPG(t, image, connectionUrl)
	return port, func() {teardownHasura(); teardownPG()}
}

func StartHasuraCLIMigrations(t TestingT, image string, pgConnectionUrl string, metadataDir, migrationsDir string) (port string, teardown func()) {
  port, teardownHasura := StartHasuraWithPG(t, image, pgConnectionUrl, func(o *docker.HostConfig) {
	o.Binds = []string{}
	if len(metadataDir) > 0 {
	  o.Binds = append(o.Binds, fmt.Sprintf("%s:%s", metadataDir, "/hasura-metadata"))
	}
	if len(migrationsDir) > 0 {
	  o.Binds = append(o.Binds, fmt.Sprintf("%s:%s", migrationsDir, "/hasura-migrations"))
	}
  })
  return port, func() {teardownHasura()}
}

func StartHasuraWithPG(t TestingT, image string, pgConnectionUrl string, dockerOpts ...func(*docker.HostConfig)) (port string, teardown func()) {
	if len(image) == 0 {
		t.Fatal("no hasura image provided, probably use testutil.HasuraDockerImage")
	}
	var err error
	pool, err := dockertest.NewPool("")
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	envs := []string{
		fmt.Sprintf("HASURA_GRAPHQL_DATABASE_URL=%s", pgConnectionUrl),
		`HASURA_GRAPHQL_ENABLE_CONSOLE=true`,
		"HASURA_GRAPHQL_DEV_MODE=true",
		"HASURA_GRAPHQL_ENABLED_LOG_TYPES=startup, http-log, webhook-log, websocket-log, query-log",
	}
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if len(adminSecret) > 0 {
		envs = append(envs, fmt.Sprintf("HASURA_GRAPHQL_ADMIN_SECRET=%s", adminSecret))
	}
	repo, tag := getDockerRepoAndTag(t, image)
	hasuraopts := &dockertest.RunOptions{
		Name:         fmt.Sprintf("%s-%s", randomdata.SillyName(), "hasura"),
		Repository:   repo,
		Tag:          tag,
		Env:          envs,
		ExposedPorts: []string{"8080/tcp"},
	}
	hasura, err := pool.RunWithOptions(hasuraopts, dockerOpts...)
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
	}
	return hasura.GetPort("8080/tcp"), teardown
}

func StartHasuraWithMetadataDatabase(t TestingT, image string) (port string, teardown func()) {
	if len(image) == 0 {
		t.Fatal("no hasura image provided, probably use testutil.HasuraDockerImage")
	}
	var err error
	pool, err := dockertest.NewPool("")
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	uniqueName := getUniqueName(t)
	pgopts := &dockertest.RunOptions{
		Name:       fmt.Sprintf("%s-%s", uniqueName, "pg"),
		Repository: "postgres",
		Tag:        "11",
		Env: []string{
			"POSTGRES_PASSWORD=postgrespassword",
			"POSTGRES_DB=postgres",
		},
	}
	pg, err := pool.RunWithOptions(pgopts)
	if err != nil {
		t.Fatalf("Could not start resource: %s", err)
	}
	var db *sql.DB
	if err = pool.Retry(func() error {
		var err error
		db, err = sql.Open("postgres", fmt.Sprintf("postgres://postgres:postgrespassword@%s:%s/%s?sslmode=disable", "0.0.0.0", pg.GetPort("5432/tcp"), "postgres"))
		if err != nil {
			return err
		}
		return db.Ping()
	}); err != nil {
		t.Fatal(err)
	}
	envs := []string{
		fmt.Sprintf("HASURA_GRAPHQL_METADATA_DATABASE_URL=postgres://postgres:postgrespassword@%s:%s/postgres", DockerSwitchIP, pg.GetPort("5432/tcp")),
		`HASURA_GRAPHQL_ENABLE_CONSOLE=true`,
		"HASURA_GRAPHQL_DEV_MODE=true",
		"HASURA_GRAPHQL_ENABLED_LOG_TYPES=startup, http-log, webhook-log, websocket-log, query-log",
	}
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if len(adminSecret) > 0 {
		envs = append(envs, fmt.Sprintf("HASURA_GRAPHQL_ADMIN_SECRET=%s", adminSecret))
	}
	repo, tag := getDockerRepoAndTag(t, image)
	hasuraopts := &dockertest.RunOptions{
		Name:         fmt.Sprintf("%s-%s", uniqueName, "hasura"),
		Repository:   repo,
		Tag:          tag,
		Env:          envs,
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

// starts a hasura instance with a metadata database and a msssql source
// returns the mssql port, source name and teardown function
func StartHasuraWithMSSQLSource(t *testing.T, version string) (string, string, func()) {
	hasuraPort, hasuraTeardown := StartHasuraWithMetadataDatabase(t, version)
	sourcename := randomdata.SillyName()
	mssqlPort, mssqlTeardown := startMSSQLContainer(t)

	teardown := func() {
		hasuraTeardown()
		mssqlTeardown()
	}
	connectionString := fmt.Sprintf("DRIVER={ODBC Driver 17 for SQL Server};SERVER=%s,%s;DATABASE=master;Uid=SA;Pwd=%s;Encrypt=no", DockerSwitchIP, mssqlPort, MSSQLPassword)
	addMSSQLSourceToHasura(t, fmt.Sprintf("%s:%s", BaseURL, hasuraPort), connectionString, sourcename)
	return hasuraPort, sourcename, teardown
}

// startsMSSQLContainer and creates a database and returns the port number
func startMSSQLContainer(t *testing.T) (string, func()) {
	pool, err := dockertest.NewPool("")
	pool.MaxWait = time.Minute
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	opts := &dockertest.RunOptions{
		Name:       fmt.Sprintf("%s-%s", randomdata.SillyName(), "mssql"),
		Repository: "mcr.microsoft.com/mssql/server",
		Tag:        "2019-latest",
		Env: []string{
			"ACCEPT_EULA=Y",
			fmt.Sprintf("SA_PASSWORD=%s", MSSQLPassword),
		},
		ExposedPorts: []string{"1433/tcp"},
	}
	mssql, err := pool.RunWithOptions(opts)
	if err != nil {
		t.Fatalf("Could not start resource: %s", err)
	}
	if err = pool.Retry(func() error {
		connString := fmt.Sprintf("server=%s;user id=%s;password=%s;port=%s;database=%s;",
			"0.0.0.0", "SA", MSSQLPassword, mssql.GetPort("1433/tcp"), "master")
		db, err := sql.Open("sqlserver", connString)
		if err != nil {
			return err
		}
		ctx := context.Background()
		err = db.PingContext(ctx)
		if err != nil {
			return err
		}
		return nil
	}); err != nil {
		t.Fatal(err)
	}
	teardown := func() {
		if err = pool.Purge(mssql); err != nil {
			t.Fatalf("Could not purge resource: %s", err)
		}
	}
	return mssql.GetPort("1433/tcp"), teardown
}

func StartPGContainer(t TestingT) (connectionString string, teardown func()) {
	user := "test"
	password := "test"
	database := "test"
	var err error
	pool, err := dockertest.NewPool("")
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	uniqueName := getUniqueName(t)
	pgopts := &dockertest.RunOptions{
		Name:       fmt.Sprintf("%s-%s", uniqueName, "pg"),
		Repository: "postgres",
		Tag:        "11",
		Env: []string{
			fmt.Sprintf("POSTGRES_USER=%s", user),
			fmt.Sprintf("POSTGRES_PASSWORD=%s", password),
			fmt.Sprintf("POSTGRES_DB=%s", database),
		},
		ExposedPorts: []string{"5432"},
	}
	pg, err := pool.RunWithOptions(pgopts)
	if err != nil {
		t.Fatalf("Could not start resource: %s", err)
	}
	var db *sql.DB
	if err = pool.Retry(func() error {
		var err error
		db, err = sql.Open("postgres", fmt.Sprintf("postgres://test:test@%s:%s/%s?sslmode=disable", "0.0.0.0", pg.GetPort("5432/tcp"), "test"))
		if err != nil {
			return err
		}
		return db.Ping()
	}); err != nil {
		t.Fatal(err)
	}
	teardown = func() {
		if err = pool.Purge(pg); err != nil {
			t.Fatalf("Could not purge resource: %s", err)
		}
	}
	connectionString = fmt.Sprintf("postgres://test:test@%s:%s/test", DockerSwitchIP, pg.GetPort("5432/tcp"))
	return connectionString, teardown
}

func addMSSQLSourceToHasura(t *testing.T, hasuraEndpoint, connectionString, sourceName string) {
	url := fmt.Sprintf("%s/v1/metadata", hasuraEndpoint)
	body := fmt.Sprintf(`
{
  "type": "mssql_add_source",
  "args": {
    "name": "%s",
    "configuration": {
        "connection_info": {
            "connection_string": "%s"
        }
    }
  }
}
`, sourceName, connectionString)
	fmt.Println(connectionString)
	fmt.Println(hasuraEndpoint)

	req, err := http.NewRequest("POST", url, strings.NewReader(body))
	require.NoError(t, err)
	req.Header.Set("Content-Type", "application/json")
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if adminSecret != "" {
		req.Header.Set("x-hasura-admin-secret", adminSecret)
	}

	r, err := http.DefaultClient.Do(req)
	require.NoError(t, err)
	if r.StatusCode != http.StatusOK {
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			t.Fatal(err)
		}
		defer r.Body.Close()
		t.Fatalf("cannot add mssql source to hasura: %s", string(body))
	}
}

func AddPGSourceToHasura(t TestingT, hasuraEndpoint, connectionString, sourceName string) {
	url := fmt.Sprintf("%s/v1/metadata", hasuraEndpoint)
	body := fmt.Sprintf(`
{
  "type": "pg_add_source",
  "args": {
    "name": "%s",
    "configuration": {
        "connection_info": {
            "database_url": "%s"
        }
    }
  }
}
`, sourceName, connectionString)
	fmt.Println(connectionString)
	fmt.Println(hasuraEndpoint)

	req, err := http.NewRequest("POST", url, strings.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	req.Header.Set("Content-Type", "application/json")
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if adminSecret != "" {
		req.Header.Set("x-hasura-admin-secret", adminSecret)
	}

	r, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	if r.StatusCode != http.StatusOK {
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			t.Fatal(err)
		}
		defer r.Body.Close()
		t.Fatalf("cannot add pg source to hasura: %s", string(body))
	}
}

func NewHttpcClient(t *testing.T, port string, headers map[string]string) *httpc.Client {
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if headers == nil {
		headers = make(map[string]string)
	}
	if len(adminSecret) > 0 {
		headers["x-hasura-admin-secret"] = adminSecret
	}
	c, err := httpc.New(nil, fmt.Sprintf("%s:%s/", BaseURL, port), headers)
	if err != nil {
		t.Fatal(err)
	}
	return c
}

func getUniqueName(t TestingT) string {
	u, err := uuid.NewV4()
	// assert.NoError(t, err)
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	return u.String() + "-" + randomdata.SillyName()
}

func StartHasuraWithCitusSource(t TestingT, image string) (string, string, func()) {
	hasuraPort, hasuraTeardown := StartHasuraWithMetadataDatabase(t, image)
	sourcename := randomdata.SillyName()
	connectionString, citusTeardown := StartCitusContainer(t)

	teardown := func() {
		hasuraTeardown()
		citusTeardown()
	}
	AddCitusSourceToHasura(t, fmt.Sprintf("%s:%s", BaseURL, hasuraPort), connectionString, sourcename)
	return hasuraPort, sourcename, teardown
}

// StartCitusContainer and creates a database and returns the port number
func StartCitusContainer(t TestingT) (string, func()) {
	password := "test"
	var err error
	pool, err := dockertest.NewPool("")
	if err != nil {
		t.Fatalf("Could not connect to docker: %s", err)
	}
	uniqueName := getUniqueName(t)
	pgopts := &dockertest.RunOptions{
		Name:       fmt.Sprintf("%s-%s", uniqueName, "pg"),
		Repository: "citusdata/citus",
		Tag:        "latest",
		Env: []string{
			fmt.Sprintf("POSTGRES_PASSWORD=%s", password),
		},
		ExposedPorts: []string{"5432"},
	}
	pg, err := pool.RunWithOptions(pgopts)
	if err != nil {
		t.Fatalf("Could not start resource: %s", err)
	}
	var db *sql.DB
	if err = pool.Retry(func() error {
		var err error
		db, err = sql.Open("postgres", fmt.Sprintf("postgres://postgres:test@%s:%s/%s?sslmode=disable", "0.0.0.0", pg.GetPort("5432/tcp"), "postgres"))
		if err != nil {
			return err
		}
		return db.Ping()
	}); err != nil {
		t.Fatal(err)
	}
	teardown := func() {
		if err = pool.Purge(pg); err != nil {
			t.Fatalf("Could not purge resource: %s", err)
		}
	}
	connectionString := fmt.Sprintf("postgres://postgres:test@%s:%s/postgres", DockerSwitchIP, pg.GetPort("5432/tcp"))
	return connectionString, teardown
}

func AddCitusSourceToHasura(t TestingT, hasuraEndpoint, connectionString, sourceName string) {
	url := fmt.Sprintf("%s/v1/metadata", hasuraEndpoint)
	body := fmt.Sprintf(`
{
  "type": "citus_add_source",
  "args": {
    "name": "%s",
    "configuration": {
        "connection_info": {
            "database_url": "%s"
        }
    }
  }
}
`, sourceName, connectionString)
	fmt.Println(connectionString)
	fmt.Println(hasuraEndpoint)
	req, err := http.NewRequest(http.MethodPost, url, strings.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	req.Header.Set("Content-Type", "application/json")
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if adminSecret != "" {
		req.Header.Set("x-hasura-admin-secret", adminSecret)
	}

	r, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	if r.StatusCode != http.StatusOK {
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			t.Fatal(err)
		}
		defer r.Body.Close()
		t.Fatalf("cannot add citus source to hasura: %s", string(body))
	}
}
