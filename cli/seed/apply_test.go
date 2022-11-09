package seed

import (
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/pgdump"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v2query"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1query"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/spf13/afero"
)

func TestDriver_ApplySeedsToDatabase(t *testing.T) {
	port13, teardown := testutil.StartHasura(t, "hasura/graphql-engine:v1.3.3")
	defer teardown()
	portLatest, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	portCitus, citusSource, teardown := testutil.StartHasuraWithCitusSource(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		SendBulk     sendBulk
		PGDumpClient hasura.PGDump
	}
	type args struct {
		fs                 afero.Fs
		rootSeedsDirectory string
		filenames          []string
		source             cli.Source
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		wantErr   bool
		before    func(t *testing.T) // functions which should be run before the test possibly to prepare test fixtures maybe
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can apply seeds in hasura/graphql-engine:v1.3.3",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, port13, nil)
					return v1query.New(c, "v1/query").Bulk
				}(),
				func() hasura.PGDump {
					c := testutil.NewHttpcClient(t, port13, nil)
					return pgdump.New(c, "v1alpha1/pg_dump")
				}(),
			},
			args{
				fs:                 afero.NewOsFs(),
				rootSeedsDirectory: "testdata/seeds",
				filenames:          []string{},
			},
			false,
			nil,
			require.NoError,
		},
		{
			"can apply seeds in latest",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, portLatest, nil)
					return v2query.New(c, "v2/query").Bulk
				}(),
				func() hasura.PGDump {
					c := testutil.NewHttpcClient(t, portLatest, nil)
					return pgdump.New(c, "v1alpha1/pg_dump")
				}(),
			},
			args{
				fs:                 afero.NewOsFs(),
				rootSeedsDirectory: "testdata/seeds",
				filenames:          []string{},
			},
			false,
			nil,
			require.NoError,
		},
		{
			"can apply seeds from files",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, portLatest, nil)
					return v2query.New(c, "v2/query").Bulk
				}(),
				func() hasura.PGDump {
					c := testutil.NewHttpcClient(t, portLatest, nil)
					return pgdump.New(c, "v1alpha1/pg_dump")
				}(),
			},
			args{
				fs:                 afero.NewOsFs(),
				rootSeedsDirectory: "testdata/seeds",
				filenames: []string{
					"articles.sql",
				},
			},
			false,
			func(t *testing.T) {
				c := testutil.NewHttpcClient(t, portLatest, nil)
				v2QueryClient := v2query.New(c, "v2/query")
				_, err := v2QueryClient.PGRunSQL(hasura.PGRunSQLInput{
					SQL:    "DROP TABLE articles",
					Source: "default",
				})
				require.NoError(t, err)
			},
			require.NoError,
		},
		{
			"can apply seeds in citus",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, portCitus, nil)
					return v2query.New(c, "v2/query").Bulk
				}(),
				func() hasura.PGDump {
					c := testutil.NewHttpcClient(t, portCitus, nil)
					return pgdump.New(c, "v1alpha1/pg_dump")
				}(),
			},
			args{
				fs: func() afero.Fs {
					fs := afero.NewMemMapFs()
					b, err := ioutil.ReadFile("testdata/seeds/articles.sql")
					require.NoError(t, err)
					err = afero.WriteFile(fs, filepath.Join("testdata/seeds/", citusSource, "articles.sql"), b, 0755)
					require.NoError(t, err)
					return fs
				}(),
				source:             cli.Source{Name: citusSource, Kind: hasura.SourceKindCitus},
				rootSeedsDirectory: "testdata/seeds",
				filenames:          []string{},
			},
			false,
			nil,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d := &Driver{
				SendBulk:     tt.fields.SendBulk,
				PGDumpClient: tt.fields.PGDumpClient,
			}
			if tt.before != nil {
				tt.before(t)
			}
			err := d.ApplySeedsToDatabase(tt.args.fs, tt.args.rootSeedsDirectory, tt.args.filenames, tt.args.source)
			tt.assertErr(t, err)
		})
	}
}
