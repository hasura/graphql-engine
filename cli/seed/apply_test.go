package seed

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/internal/hasura/pgdump"
	"github.com/hasura/graphql-engine/cli/internal/testutil"

	"github.com/hasura/graphql-engine/cli/internal/hasura/v1query"
	"github.com/hasura/graphql-engine/cli/internal/httpc"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/hasura"
	"github.com/spf13/afero"
)

func TestDriver_ApplySeedsToDatabase(t *testing.T) {
	port13, teardown := testutil.StartHasura(t, "v1.3.3")
	defer teardown()
	portLatest, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
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
			"can apply seeds in v1.3.3",
			fields{
				func() sendBulk {
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", port13), nil)
					if err != nil {
						t.Fatal(err)
					}
					return v1query.New(c, "v1/query").Bulk
				}(),
				func() hasura.PGDump {
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", port13), nil)
					if err != nil {
						t.Fatal(err)
					}
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
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", portLatest), nil)
					if err != nil {
						t.Fatal(err)
					}
					return v1query.New(c, "v2/query").Bulk
				}(),
				func() hasura.PGDump {
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", portLatest), nil)
					if err != nil {
						t.Fatal(err)
					}
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
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", portLatest), nil)
					if err != nil {
						t.Fatal(err)
					}
					return v1query.New(c, "v2/query").Bulk
				}(),
				func() hasura.PGDump {
					c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", portLatest), nil)
					if err != nil {
						t.Fatal(err)
					}
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
				c, err := httpc.New(nil, fmt.Sprintf("http://localhost:%s/", portLatest), nil)
				if err != nil {
					t.Fatal(err)
				}
				v1QueryClient := v1query.New(c, "v2/query")
				_, err = v1QueryClient.PGRunSQL(hasura.PGRunSQLInput{
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
