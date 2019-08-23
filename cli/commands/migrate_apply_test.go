package commands

import (
	"math/rand"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/sirupsen/logrus/hooks/test"
)

func testMigrateApply(t *testing.T, endpoint *url.URL, migrationsDir string, up string, down string, v string, vType string) {
	logger, _ := test.NewNullLogger()
	opts := &migrateApplyOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MigrationDir: migrationsDir,
			ServerConfig: &cli.ServerConfig{
				Endpoint:       endpoint.String(),
				AdminSecret:    os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
				ParsedEndpoint: endpoint,
			},
		},
		upMigration:      up,
		downMigration:    down,
		versionMigration: v,
		migrationType:    vType,
	}

	opts.EC.Version = version.New()
	v, err := version.FetchServerVersion(opts.EC.ServerConfig.Endpoint)
	if err != nil {
		t.Fatalf("getting server version failed: %v", err)
	}
	opts.EC.Version.SetServerVersion(v)

	err = opts.run()
	if err != nil {
		t.Fatalf("failed applying migration: %v", err)
	}
}

func TestMigrateApplyWithInvalidEndpoint(t *testing.T) {
	logger, _ := test.NewNullLogger()
	opts := &migrateApplyOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MigrationDir: filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
			ServerConfig: &cli.ServerConfig{
				Endpoint:       ":",
				AdminSecret:    "",
				ParsedEndpoint: &url.URL{},
			},
		},
	}

	opts.EC.Version = version.New()
	v, err := version.FetchServerVersion(opts.EC.ServerConfig.Endpoint)
	if err == nil {
		t.Fatalf("expected error to be not nil")
	}
	opts.EC.Version.SetServerVersion(v)
	err = opts.run()
	if err == nil {
		t.Fatalf("expected err not to be nil")
	}
}

func TestMigrateApplyWithMultipleFlags(t *testing.T) {
	logger, _ := test.NewNullLogger()
	opts := &migrateApplyOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MigrationDir: filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
			ServerConfig: &cli.ServerConfig{
				Endpoint:    ":",
				AdminSecret: "",
			},
		},
		upMigration:   "1",
		downMigration: "2",
	}

	opts.EC.Version = version.New()
	opts.EC.Version.SetServerVersion("")

	err := opts.EC.ServerConfig.ParseEndpoint()
	if err == nil {
		t.Fatalf("expected err not to be nil")
	}

	err = opts.run()
	if err == nil {
		t.Fatalf("expected err not to be nil")
	}
}
