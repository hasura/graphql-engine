package v2

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/commands"

	"github.com/spf13/afero"

	"github.com/hasura/graphql-engine/cli/seed"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/stretchr/testify/assert"
)

type seedCreateInterface interface {
	Run() error
}

func TestSeedsCreateCmd(t *testing.T, ec *cli.ExecutionContext) {
	// copy migrations to ec.Execution.Directory/migrations
	os.RemoveAll(ec.SeedsDirectory)
	currDir, _ := os.Getwd()
	err := util.CopyDir(filepath.Join(currDir, "v2/seeds"), ec.SeedsDirectory)
	if err != nil {
		t.Fatalf("unable to copy migrations directory %v", err)
	}
	type args struct {
		fs   afero.Fs
		opts seed.CreateSeedOpts
	}
	tt := []struct {
		name         string
		args         args
		wantErr      bool
		wantFilepath *string
	}{
		{
			"can create a seed a file",
			args{
				fs: afero.NewMemMapFs(),
				opts: seed.CreateSeedOpts{
					DirectoryPath:        "seeds/",
					Data:                 strings.NewReader("INSERT INTO account1 (username, password, email) values ('scriptonist', 'no you cant guess it', 'hello@drogon.com');"),
					UserProvidedSeedName: "can_we_create_seed_files",
				},
			},
			false,
			func() *string { s := "test/test"; return &s }(),
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			var inData bytes.Buffer
			tc.args.opts.Data = io.TeeReader(tc.args.opts.Data, &inData)
			gotFilename, err := seed.CreateSeedFile(tc.args.fs, tc.args.opts)
			if (err != nil) && !tc.wantErr {
				t.Errorf("CreateSeedFile() error = %v, wantErr %v", err, tc.wantErr)
				return
			}

			if gotFilename == nil {
				return
			}
			// Do a regex match for filename returned
			// check if it is in required format
			var re = regexp.MustCompile(`^([a-z]+\/)([0-9]+)\_(.+)(\.sql)$`)
			regexGroups := re.FindStringSubmatch(*gotFilename)

			// Since filename has to be in form
			// dirname/21212_filename.sql
			// regexGroups should have 5 elements
			// element 0: whole string
			// element 1: dirname
			// element 2: timestamp
			// element 3: filename
			// element 4: extension
			if len(regexGroups) != 5 {
				t.Fatalf("CreateSeedFile() = %v, but want filepath of form"+` [a-z]+\/[0-9]+\_[a-zA-Z]+\.sql`, *gotFilename)
			}
			gotDirectoryPath := regexGroups[1]
			gotUserProvidedFilename := regexGroups[3]
			gotFileExtension := regexGroups[4]

			assert.Equal(t, gotDirectoryPath, tc.args.opts.DirectoryPath)
			assert.Equal(t, gotUserProvidedFilename, tc.args.opts.UserProvidedSeedName)
			assert.Equal(t, gotFileExtension, ".sql")

			// test if a filewith the filename was created
			if s, err := tc.args.fs.Stat(*gotFilename); err != nil {
				if s.IsDir() {
					t.Fatalf("expected to get a file with name %v", *gotFilename)
				}
			}

			// check if the contents match
			gotBytes, err := afero.ReadFile(tc.args.fs, *gotFilename)
			assert.NoError(t, err)
			assert.Equal(t, string(gotBytes), string(inData.Bytes()))
		})
	}
}
func TestSeedsApplyCmd(t *testing.T, ec *cli.ExecutionContext) {
	// copy migrations to ec.Execution.Directory/migrations
	os.RemoveAll(ec.SeedsDirectory)
	currDir, _ := os.Getwd()
	err := util.CopyDir(filepath.Join(currDir, "v2/seeds"), ec.SeedsDirectory)
	if err != nil {
		t.Fatalf("unable to copy migrations directory %v", err)
	}
	tt := []struct {
		name    string
		opts    seedCreateInterface
		wantErr bool
	}{
		{
			"can apply all seeds",
			&commands.SeedApplyOptions{
				EC: ec,
			},
			false,
		},
		{
			"can apply single file",
			&commands.SeedApplyOptions{
				EC:        ec,
				FileNames: []string{"1591867862409_test.sql"},
			},
			false,
		},
		{
			"throws error when applying no idempotent operations",
			&commands.SeedApplyOptions{
				EC:        ec,
				FileNames: []string{"1591867862419_test2.sql"},
			},
			true,
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.opts.Run()
			if (err != nil) && (tc.wantErr == false) {
				t.Fatalf("%s: expected no error got %v", tc.name, err)
			}
		})
	}
}
