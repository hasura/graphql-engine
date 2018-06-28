package commands

import (
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/stretchr/testify/assert"
)

func testMigrateStatus(t *testing.T, endpoint string, migrationsDir string, expectedStatus string) {
	logger, hook := test.NewNullLogger()
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

	err := opts.Run()
	if err != nil {
		t.Fatalf("failed fetching migration status: %v", err)
	}
	assert.Equal(t, expectedStatus, hook.LastEntry().Message)
}
