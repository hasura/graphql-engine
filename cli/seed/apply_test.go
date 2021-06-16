package seed

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/pgdump"
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
		name    string
		fields  fields
		args    args
		wantErr bool
		// functions which should be run before the test
		// possibly to prepare test fixtures maybe
		before func(t *testing.T)
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
		},
		{
			"can apply seeds in latest",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, portLatest, nil)
					return v1query.New(c, "v2/query").Bulk
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
		},
		{
			"can apply seeds from files",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, portLatest, nil)
					return v1query.New(c, "v2/query").Bulk
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
				v1QueryClient := v1query.New(c, "v2/query")
				_, err := v1QueryClient.PGRunSQL(hasura.PGRunSQLInput{
					SQL:    "DROP TABLE articles",
					Source: "default",
				})
				require.NoError(t, err)
			},
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
			if err := d.ApplySeedsToDatabase(tt.args.fs, tt.args.rootSeedsDirectory, tt.args.filenames, tt.args.source); (err != nil) != tt.wantErr {
				t.Errorf("ApplySeedsToDatabase() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
