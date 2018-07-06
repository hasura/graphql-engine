package commands

import (
	"math/rand"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/stretchr/testify/assert"
)

func testMigrateStatus(t *testing.T, endpoint string, migrationsDir string, expectedStatus *migrate.Status) {
	logger, _ := test.NewNullLogger()
	opts := &migrateStatusOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MigrationDir: migrationsDir,
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:  endpoint,
				AccessKey: "",
			},
		},
	}

	status, err := opts.run()
	if err != nil {
		t.Fatalf("failed fetching migration status: %v", err)
	}
	assert.Equal(t, expectedStatus, status)
}

func TestMigrateStatusWithInvalidEndpoint(t *testing.T) {
	logger, _ := test.NewNullLogger()
	opts := &migrateStatusOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MigrationDir: filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:  ":",
				AccessKey: "",
			},
		},
	}

	_, err := opts.run()
	if err == nil {
		t.Fatalf("expected err not to be nil")
	}
}
