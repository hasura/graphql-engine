package v1

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/commands"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/stretchr/testify/assert"
)

type metadataInterface interface {
	Run() error
}

func TestMetadataCmd(t *testing.T, ec *cli.ExecutionContext) {
	currDir, _ := os.Getwd()
	actualMetadataFile := filepath.Join(ec.MigrationDir, "metadata.yaml")
	tt := []struct {
		name                 string
		opts                 metadataInterface
		err                  error
		copyMetadataFile     string
		expectedMetadataFile string
	}{
		{
			"metadata-export",
			&commands.MetadataExportOptions{
				EC:         ec,
				ActionType: "export",
			},
			nil,
			"",
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "1_metadata.yaml"),
		},
		{
			"metadata-apply",
			&commands.MetadataApplyOptions{
				EC:         ec,
				ActionType: "apply",
			},
			nil,
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "2_metadata.yaml"),
			"",
		},
		{
			"metadata-export",
			&commands.MetadataExportOptions{
				EC:         ec,
				ActionType: "export",
			},
			nil,
			"",
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "2_metadata.yaml"),
		},
		{
			"metadata-clear",
			&commands.MetadataClearOptions{
				EC:         ec,
				ActionType: "clear",
			},
			nil,
			"",
			"",
		},
		{
			"metadata-export",
			&commands.MetadataExportOptions{
				EC:         ec,
				ActionType: "export",
			},
			nil,
			"",
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "1_metadata.yaml"),
		},
		{
			"metadata-diff",
			&commands.MetadataDiffOptions{
				EC:     ec,
				Args:   []string{filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "2_metadata.yaml")},
				Output: new(bytes.Buffer),
			},
			nil,
			"",
			"",
		},
		{
			"down-all-migrations",
			&commands.MigrateApplyOptions{
				EC:            ec,
				DownMigration: "all",
			},
			nil,
			"",
			"",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			if tc.copyMetadataFile != "" {
				err := util.CopyFile(tc.copyMetadataFile, actualMetadataFile)
				if err != nil {
					t.Fatalf("%s: unable to copy metadata file, got %v", tc.name, err)
				}
			}
			err := tc.opts.Run()
			if err != tc.err {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			}
			if tc.expectedMetadataFile != "" {
				assert.FileExists(t, actualMetadataFile)
				expectedByt, err := ioutil.ReadFile(tc.expectedMetadataFile)
				if err != nil {
					t.Fatalf("%s: unable to read expected metadata file, got %v", tc.name, err)
				}
				actualByt, err := ioutil.ReadFile(actualMetadataFile)
				if err != nil {
					t.Fatalf("%s: unable to read actual metadata file, got %v", tc.name, err)
				}
				assert.Equal(t, string(expectedByt), string(actualByt))
			}
		})
	}
}
