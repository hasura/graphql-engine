package scripts

import (
	"os"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore/migrations"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore/settings"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1metadata"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1query"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v2query"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/spf13/afero"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test_checkIfDirectoryIsMigration(t *testing.T) {
	type args struct {
		dirPath string
	}
	tests := []struct {
		name      string
		args      args
		want      bool
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can check if a directory name is a valid migration",
			args{
				dirPath: "testdata/1604855964903_test",
			},
			true,
			false,
			require.NoError,
		},
		{
			"can check if a directory name is a valid migration",
			args{
				dirPath: "1604855964903_test",
			},
			true,
			false,
			require.NoError,
		},
		{
			"can check if a directory name is a valid migration",
			args{
				dirPath: "testdata/160855964903_test",
			},
			false,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := isHasuraCLIGeneratedMigration(tt.args.dirPath)
			tt.assertErr(t, err)
			if tt.wantErr {
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
		name      string
		args      args
		want      []string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
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
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := getMigrationDirectoryNames(tt.args.fs, tt.args.rootMigrationsDir)
			tt.assertErr(t, err)
			if tt.wantErr {
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
		name      string
		args      args
		want      []string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
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
			[]string{"moved/1", "moved/2", "moved/3"},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := copyMigrations(tt.args.fs, tt.args.dirs, tt.args.parentMigrationsDirectory, tt.args.target)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
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
		name      string
		args      args
		wantErr   bool
		assertErr require.ErrorAssertionFunc
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
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := removeDirectories(tt.args.fs, tt.args.parentDirectory, tt.args.dirs)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			for _, d := range tt.args.dirs {
				_, err := tt.args.fs.Stat(d)
				assert.Error(t, err)
			}
		})
	}
}

func Test_copyState(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	type args struct {
		ec           *cli.ExecutionContext
		destdatabase string
	}
	tests := []struct {
		name      string
		args      args
		wantErr   bool
		assertErr require.ErrorAssertionFunc
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
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			srcSettings := cli.GetSettingsStateStore(tt.args.ec, "default")
			assert.NoError(t, srcSettings.PrepareSettingsDriver())
			srcMigrations := cli.GetMigrationsStateStore(tt.args.ec)
			assert.NoError(t, srcMigrations.PrepareMigrationsStateStore("default"))

			dstSettings := settings.NewStateStoreCatalog(statestore.NewCLICatalogState(tt.args.ec.APIClient.V1Metadata))
			dstMigrations := migrations.NewCatalogStateStore(statestore.NewCLICatalogState(tt.args.ec.APIClient.V1Metadata))
			assert.NoError(t, srcSettings.UpdateSetting("test", "test"))
			assert.NoError(t, srcMigrations.SetVersion("", 123, false))
			err := CopyState(tt.args.ec, "default", tt.args.destdatabase)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
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
