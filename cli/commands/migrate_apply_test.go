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
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/stretchr/testify/assert"
)

func testMigrateApply(t *testing.T, endpoint string, migrationsDir string, up string, down string, version string, versionType string) {
	logger, hook := test.NewNullLogger()
	opts := &migrateApplyOptions{
		EC: &cli.ExecutionContext{
			Logger:       logger,
			Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
			MigrationDir: migrationsDir,
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:  endpoint,
				AccessKey: "",
			},
		},
		upMigration:      up,
		downMigration:    down,
		versionMigration: version,
		migrationType:    versionType,
	}

	err := opts.run()
	if err != nil {
		t.Fatalf("failed applying migration: %v", err)
	}

	assert.Equal(t, "migrations applied", hook.LastEntry().Message)
}

func TestMigrateApplyWithInvalidEndpoint(t *testing.T) {
	logger, _ := test.NewNullLogger()
	opts := &migrateApplyOptions{
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

	err := opts.run()
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
			Config: &cli.HasuraGraphQLConfig{
				Endpoint:  ":",
				AccessKey: "",
			},
		},
		upMigration:   "1",
		downMigration: "2",
	}

	err := opts.run()
	if err == nil {
		t.Fatalf("expected err not to be nil")
	}
}
