package v2

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/stretchr/testify/assert"
)

type metadataInterface interface {
	Run() error
}

func TestMetadataCmd(t *testing.T, ec *cli.ExecutionContext) {
	currDir, _ := os.Getwd()
	tt := []struct {
		name                   string
		opts                   metadataInterface
		err                    error
		copyMetadataFolder     string
		expectedMetadataFolder string
	}{
		{
			"metadata-export",
			&commands.MetadataExportOptions{
				EC: ec,
			},
			nil,
			"",
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "1_metadata"),
		},
		{
			"metadata-apply",
			&commands.MetadataApplyOptions{
				EC: ec,
			},
			nil,
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "2_metadata"),
			"",
		},
		{
			"metadata-export",
			&commands.MetadataExportOptions{
				EC: ec,
			},
			nil,
			"",
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "2_metadata"),
		},
		{
			"metadata-clear",
			&commands.MetadataClearOptions{
				EC: ec,
			},
			nil,
			"",
			"",
		},
		{
			"metadata-export",
			&commands.MetadataExportOptions{
				EC: ec,
			},
			nil,
			"",
			filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "1_metadata"),
		},
		{
			"metadata-diff",
			&commands.MetadataDiffOptions{
				EC:     ec,
				Args:   []string{filepath.Join(currDir, getMetadataDir(ec.Version.ServerSemver), "1_metadata")},
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
				Source:        cli.Source{Kind: hasura.SourceKindPG},
			},
			nil,
			"",
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
				files, err := ioutil.ReadDir(tc.expectedMetadataFolder)
				if err != nil {
					t.Fatalf("%s: unable to read expected metadata directory, got %v", tc.name, err)
				}

				for _, file := range files {
					name := file.Name()
					expectedByt, err := ioutil.ReadFile(filepath.Join(tc.expectedMetadataFolder, name))
					if err != nil {
						t.Fatalf("%s: unable to read expected metadata file %s, got %v", tc.name, name, err)
					}
					actualByt, err := ioutil.ReadFile(filepath.Join(ec.MetadataDir, name))
					if err != nil {
						t.Fatalf("%s: unable to read actual metadata file %s, got %v", tc.name, name, err)
					}
					assert.Equalf(t, string(expectedByt), string(actualByt), "file: %s", filepath.Join(tc.expectedMetadataFolder, name))
				}
			}
		})
	}
}
