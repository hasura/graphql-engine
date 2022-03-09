package v3

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/stretchr/testify/assert"
)

type deployInterface interface {
	Run() error
}

func TestDeployCmd(t *testing.T, ec *cli.ExecutionContext) {
	// copy migrations to ec.Execution.Directory/migrations
	os.RemoveAll(ec.MigrationDir)
	currDir, _ := os.Getwd()
	err := util.CopyDir(filepath.Join(currDir, "v3/migrations"), ec.MigrationDir)
	if err != nil {
		t.Fatalf("unable to copy migrations directory %v", err)
	}
	ec.Source.Name = "default"
	ec.Source.Kind = hasura.SourceKindPG
	tt := []struct {
		name                   string
		opts                   deployInterface
		err                    error
		migrateStatus          migrate.Status
		copyMetadataFolder     string
		expectedMetadataFolder string
	}{
		{"apply-all-metadata-and-migrations", &commands.DeployOptions{
			EC: ec,
		}, nil, migrate.Status{
			Index: []uint64{1, 2},
			Migrations: map[uint64]*migrate.MigrationStatus{
				1: {
					IsApplied: true,
					IsPresent: true,
				},
				2: {
					IsApplied: true,
					IsPresent: true,
				},
			},
		},
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "2_metadata"),
			"",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			if tc.copyMetadataFolder != "" {
				err := os.RemoveAll(ec.MetadataDir)
				if err != nil {
					t.Fatalf("%s: unable to remove metadata directory, got %v", tc.name, err)
				}
				err = util.CopyDir(tc.copyMetadataFolder, ec.MetadataDir)
				if err != nil {
					t.Fatalf("%s: unable to copy metadata file, got %v", tc.name, err)
				}
			}

			err := tc.opts.Run()
			if err != tc.err {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			}

			if tc.expectedMetadataFolder != "" {
				assert.DirExists(t, ec.MetadataDir)
				err = filepath.Walk(filepath.Join(tc.expectedMetadataFolder), func(path string, info os.FileInfo, err error) error {
					if !info.IsDir() {
						name := info.Name()
						expectedByt, err := ioutil.ReadFile(path)
						if err != nil {
							t.Fatalf("%s: unable to read expected metadata file %s, got %v", tc.name, name, err)
						}
						actualByt, err := ioutil.ReadFile(strings.Replace(path, tc.expectedMetadataFolder, ec.MetadataDir, 1))
						if err != nil {
							t.Fatalf("%s: unable to read actual metadata file %s, got %v", tc.name, name, err)
						}
						assert.Equal(t, string(expectedByt), string(actualByt))
					}
					return nil
				})
				if err != nil {
					t.Fatalf("%s: unable to read metadata, got %v", tc.name, err)
				}
			}

			expectedStatusByt, err := json.Marshal(tc.migrateStatus)
			if err != nil {
				t.Fatal(err)
			}
			statusOpts := &commands.MigrateStatusOptions{
				EC:     ec,
				Source: cli.Source{Name: "default", Kind: hasura.SourceKindPG},
			}
			actualStatus, err := statusOpts.RunOnSource()
			if err != nil {
				t.Fatalf("%s: unable to fetch migrate status, got %v", tc.name, err)
			}
			actualStatusByt, err := json.Marshal(actualStatus)
			if err != nil {
				t.Fatal(err)
			}
			assert.Equal(t, string(expectedStatusByt), string(actualStatusByt))
		})
	}
}
