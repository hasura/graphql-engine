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
	"github.com/spf13/pflag"
)

func TestMigrateCreateCmd(t *testing.T) {
	t.Run("should run create migration", func(t *testing.T) {
		logger, _ := test.NewNullLogger()
		opts := &migrateCreateOptions{
			EC: &cli.ExecutionContext{
				Logger:       logger,
				Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
				MigrationDir: filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
			},
			name:  "create_article",
			flags: pflag.NewFlagSet("migrate-create-test", pflag.ContinueOnError),
		}

		_, err := opts.run()
		if err != nil {
			t.Fatalf("failed creating migration: %v", err)
		}

		_ = os.RemoveAll(opts.EC.MigrationDir)
	})

	t.Run("should run create migration with sql files", func(t *testing.T) {
		logger, _ := test.NewNullLogger()
		opts := &migrateCreateOptions{
			EC: &cli.ExecutionContext{
				Logger:       logger,
				Spinner:      spinner.New(spinner.CharSets[7], 100*time.Millisecond),
				MigrationDir: filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
			},
			name:      "create_article",
			flags:     pflag.NewFlagSet("migrate-create-test", pflag.ContinueOnError),
			sqlFormat: true,
		}

		version, err := opts.run()
		if err != nil {
			t.Fatalf("failed creating migration: %v", err)
		}

		baseName := strconv.FormatInt(version, 10) + "_" + "create_article"
		upSqlFile := filepath.Join(opts.EC.MigrationDir, baseName+".up.sql")
		downSqlFile := filepath.Join(opts.EC.MigrationDir, baseName+".down.sql")
		_, err = os.Stat(upSqlFile)
		if err != nil {
			t.Fatalf("up sql file not present: %v", err)
		}

		_, err = os.Stat(downSqlFile)
		if err != nil {
			t.Fatalf("down sql file not present: %v", err)
		}

		_ = os.RemoveAll(opts.EC.MigrationDir)
	})
}
