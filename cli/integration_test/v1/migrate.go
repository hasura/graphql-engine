package v1

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/commands"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/stretchr/testify/assert"
)

type migrateInterface interface {
	Run() error
}

func TestMigrateCmd(t *testing.T, ec *cli.ExecutionContext) {
	// copy migrations to ec.Execution.Directory/migrations
	os.RemoveAll(ec.MigrationDir)
	currDir, _ := os.Getwd()
	err := util.CopyDir(filepath.Join(currDir, "v1/migrations"), ec.MigrationDir)
	if err != nil {
		t.Fatalf("unable to copy migrations directory %v", err)
	}

	tt := []struct {
		name   string
		opts   migrateInterface
		err    error
		status migrate.Status
	}{
		{"apply-up-all-migrations", &commands.MigrateApplyOptions{
			EC: ec,
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
			},
		}},
		{"apply-down-1-migration", &commands.MigrateApplyOptions{
			EC:            ec,
			DownMigration: "1",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
			},
		}},
		{"apply-down-all-migration", &commands.MigrateApplyOptions{
			EC:            ec,
			DownMigration: "all",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
			},
		}},
		{"apply-goto-2-migration", &commands.MigrateApplyOptions{
			EC:          ec,
			GotoVersion: "2",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
			},
		}},
		{"apply-goto-nil-migration", &commands.MigrateApplyOptions{
			EC:          ec,
			GotoVersion: "-1",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
			},
		}},
		{"apply-up-1-migration", &commands.MigrateApplyOptions{
			EC:          ec,
			UpMigration: "1",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
			},
		}},
		{"apply-version-2-up-migration", &commands.MigrateApplyOptions{
			EC:               ec,
			VersionMigration: "2",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
			},
		}},
		{"apply-version-2-down-migration", &commands.MigrateApplyOptions{
			EC:               ec,
			VersionMigration: "2",
			MigrationType:    "down",
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: &migrate.MigrationStatus{
					IsApplied: true,
					IsPresent: true,
				},
				2: &migrate.MigrationStatus{
					IsApplied: false,
					IsPresent: true,
				},
			},
		}},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.opts.Run()
			if err != tc.err {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			}

			expectedStatusByt, err := yaml.Marshal(tc.status)
			if err != nil {
				t.Fatal(err)
			}
			statusOpts := &commands.MigrateStatusOptions{
				EC: ec,
			}
			actualStatus, err := statusOpts.Run()
			if err != nil {
				t.Fatalf("%s: unable to fetch migrate status, got %v", tc.name, err)
			}
			actualStatusByt, err := yaml.Marshal(actualStatus)
			if err != nil {
				t.Fatal(err)
			}
			assert.Equal(t, string(expectedStatusByt), string(actualStatusByt))
		})
	}
}
