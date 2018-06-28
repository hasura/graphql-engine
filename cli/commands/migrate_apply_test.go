package commands

import (
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/stretchr/testify/assert"
)

func testMigrateApply(t *testing.T, endpoint string, migrationsDir string) {
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
		upMigration: "1",
	}

	err := opts.Run()
	if err != nil {
		t.Fatalf("failed applying migration: %v", err)
	}

	assert.Equal(t, "migrations applied", hook.LastEntry().Message)
}
