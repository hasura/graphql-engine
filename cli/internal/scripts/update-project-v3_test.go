package scripts

import (
	"io"
	"os"
	"reflect"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
	"github.com/hasura/graphql-engine/cli/internal/statestore"
	"github.com/hasura/graphql-engine/cli/internal/statestore/migrations"
	"github.com/hasura/graphql-engine/cli/internal/statestore/settings"

	"github.com/hasura/graphql-engine/cli/internal/hasura/v1metadata"
	"github.com/hasura/graphql-engine/cli/internal/hasura/v1query"
	"github.com/hasura/graphql-engine/cli/internal/hasura/v2query"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/hasura"
	"github.com/hasura/graphql-engine/cli/internal/testutil"

	"github.com/spf13/afero"
	"github.com/stretchr/testify/assert"
)

func Test_checkIfDirectoryIsMigration(t *testing.T) {
	type args struct {
		dirPath string
	}
	tests := []struct {
		name    string
		args    args
		want    bool
		wantErr bool
	}{
		{
			"can check if a directory name is a valid migration",
			args{
				dirPath: "testdata/1604855964903_test",
			},
			true,
			false,
		},
		{
			"can check if a directory name is a valid migration",
			args{
				dirPath: "1604855964903_test",
			},
			true,
			false,
		},
		{
			"can check if a directory name is a valid migration",
			args{
				dirPath: "testdata/160855964903_test",
			},
			false,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := isHasuraCLIGeneratedMigration(tt.args.dirPath)
			if (err != nil) != tt.wantErr {
				t.Errorf("getMigrationDirectoryNames() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			assert.Equal(t, tt.want, got)
		})
	}
}

func Test_getMigrationDirectoryNames(t *testing.T) {
	type args struct {
		fs                afero.Fs
		rootMigrationsDir string
	}
	tests := []struct {
		name    string
		args    args
		want    []string
		wantErr bool
	}{
		{
			"can get list of migration directories",
			args{
				fs: func() afero.Fs {
					fs := afero.NewMemMapFs()
					if err := fs.MkdirAll("migrations/1604855964903_test2", os.ModePerm); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("migrations/1604255964903_test", os.ModePerm); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("migrations/randomdir", os.ModePerm); err != nil {
						t.Fatal(err)
					}
					file, err := fs.Create("migrations/somefile.yaml")
					if err != nil {
						t.Fatal(err)
					}
					file.Close()
					return fs
				}(),
				rootMigrationsDir: "migrations",
			},
			[]string{
				"1604255964903_test",
				"1604855964903_test2",
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := getMigrationDirectoryNames(tt.args.fs, tt.args.rootMigrationsDir)
			if (err != nil) != tt.wantErr {
				t.Errorf("getMigrationDirectoryNames() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			assert.Equal(t, tt.want, got)
		})
	}
}

func Test_moveMigrationsToDatabaseDirectory(t *testing.T) {
	type args struct {
		fs                        afero.Fs
		dirs                      []string
		parentMigrationsDirectory string
		target                    string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
		want    []string
	}{
		{
			"can move directories to directory",
			args{
				fs: func() afero.Fs {
					fs := afero.NewMemMapFs()
					if err := fs.MkdirAll("1", 0755); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("2", 0755); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("3", 0755); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("moved", 0755); err != nil {
						t.Fatal(err)
					}

					return fs
				}(),
				dirs:                      []string{"1", "2", "3"},
				parentMigrationsDirectory: ".",
				target:                    "moved",
			},
			false,
			[]string{"moved/1", "moved/2", "moved/3"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := copyMigrations(tt.args.fs, tt.args.dirs, tt.args.parentMigrationsDirectory, tt.args.target); (err != nil) != tt.wantErr {
				assert.NoError(t, err)
			}
			for _, want := range tt.want {
				_, err := tt.args.fs.Stat(want)
				assert.NoError(t, err)
			}
		})
	}
}

func Test_removeDirectories(t *testing.T) {
	type args struct {
		fs              afero.Fs
		parentDirectory string
		dirs            []string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
	}{
		{
			"can delete directories",
			args{
				fs: func() afero.Fs {
					fs := afero.NewMemMapFs()
					if err := fs.MkdirAll("1/x/y", 0755); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("2", 0755); err != nil {
						t.Fatal(err)
					}
					if err := fs.MkdirAll("3", 0755); err != nil {
						t.Fatal(err)
					}
					return fs
				}(),
				dirs:            []string{"1", "2", "4"},
				parentDirectory: ".",
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := removeDirectories(tt.args.fs, tt.args.parentDirectory, tt.args.dirs); (err != nil) != tt.wantErr {
				t.Errorf("removeDirectories() error = %v, wantErr %v", err, tt.wantErr)
			}
			for _, d := range tt.args.dirs {
				_, err := tt.args.fs.Stat(d)
				assert.Error(t, err)
			}
		})
	}
}

func Test_copyState(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardown()
	type args struct {
		ec           *cli.ExecutionContext
		destdatabase string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
	}{
		{
			"can move state",
			args{
				func() *cli.ExecutionContext {
					return &cli.ExecutionContext{
						Config: &cli.Config{
							Version: cli.V2,
						},
						APIClient: &hasura.Client{
							V1Metadata: v1metadata.New(testutil.NewHttpcClient(t, port, nil), "v1/metadata"),
							V1Query:    v1query.New(testutil.NewHttpcClient(t, port, nil), "v1/query"),
							V2Query:    v2query.New(testutil.NewHttpcClient(t, port, nil), "v2/query"),
						},
					}
				}(),
				"test",
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			srcSettings := cli.GetSettingsStateStore(tt.args.ec)
			assert.NoError(t, srcSettings.PrepareSettingsDriver())
			srcMigrations := cli.GetMigrationsStateStore(tt.args.ec)
			assert.NoError(t, srcMigrations.PrepareMigrationsStateStore())

			dstSettings := settings.NewStateStoreCatalog(statestore.NewCLICatalogState(tt.args.ec.APIClient.V1Metadata))
			dstMigrations := migrations.NewCatalogStateStore(statestore.NewCLICatalogState(tt.args.ec.APIClient.V1Metadata))
			assert.NoError(t, srcSettings.UpdateSetting("test", "test"))
			assert.NoError(t, srcMigrations.SetVersion("", 123, false))
			if err := copyState(tt.args.ec, tt.args.destdatabase); (err != nil) != tt.wantErr {
				t.Fatalf("copyState() error = %v, wantErr %v", err, tt.wantErr)
			}
			v, err := dstSettings.GetSetting("test")
			assert.NoError(t, err)
			assert.Equal(t, "test", v)
			m, err := dstMigrations.GetVersions(tt.args.destdatabase)
			assert.NoError(t, err)
			assert.Equal(t, map[uint64]bool{123: false}, m)
		})
	}
}

func Test_listDatabases(t *testing.T) {
	type args struct {
		client hasura.CommonMetadataOperations
	}
	tests := []struct {
		name    string
		args    args
		want    []string
		wantErr bool
	}{
		{
			"can get list of sources",
			args{
				commonMetadataTest{
					`
{
	"sources": [
		{
			"name": "test1"
		},
		{

			"name": "test2"
		}
	]
}
`,
				},
			},
			[]string{"test1", "test2"},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ListDatabases(tt.args.client)
			if (err != nil) != tt.wantErr {
				t.Errorf("ListDatabases() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ListDatabases() got = %v, want %v", got, tt.want)
			}
		})
	}
}

type commonMetadataTest struct {
	s string
}

func (c commonMetadataTest) SendCommonMetadataOperation(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error) {
	panic("implement me")
}

func (c commonMetadataTest) ExportMetadata() (metadata io.Reader, err error) {
	return strings.NewReader(c.s), nil
}

func (c commonMetadataTest) ClearMetadata() (io.Reader, error) {
	panic("implement me")
}

func (c commonMetadataTest) ReloadMetadata() (io.Reader, error) {
	panic("implement me")
}

func (c commonMetadataTest) DropInconsistentMetadata() (io.Reader, error) {
	panic("implement me")
}

func (c commonMetadataTest) ReplaceMetadata(metadata io.Reader) (io.Reader, error) {
	panic("implement me")
}

func (c commonMetadataTest) GetInconsistentMetadata() (*hasura.GetInconsistentMetadataResponse, error) {
	panic("implement me")
}

func (c commonMetadataTest) GetInconsistentMetadataReader() (io.Reader, error) {
	panic("implement me")
}
