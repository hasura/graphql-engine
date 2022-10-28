package seed

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/pgdump"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/v1query"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func TestDriver_ExportDatadump(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		SendBulk     sendBulk
		PGDumpClient hasura.PGDump
	}
	type args struct {
		tableNames []string
		sourceName string
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		want      string
		wantErr   bool
		before    func(t *testing.T)
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can export data dump",
			fields{
				func() sendBulk {
					c := testutil.NewHttpcClient(t, port, nil)
					return v1query.New(c, "v2/query").Bulk
				}(),
				func() hasura.PGDump {
					c := testutil.NewHttpcClient(t, port, nil)
					return pgdump.New(c, "v1alpha1/pg_dump")
				}(),
			},
			args{
				tableNames: []string{"articles", "authors"},
				sourceName: "default",
			},
			`SET check_function_bodies = false;
INSERT INTO public.articles (id, title, content, rating, author_id) VALUES (1, 'test1', 'test1', 1, 4);
INSERT INTO public.articles (id, title, content, rating, author_id) VALUES (2, 'test2', 'test1', 1, 4);
INSERT INTO public.articles (id, title, content, rating, author_id) VALUES (3, 'test3', 'test1', 1, 4);
INSERT INTO public.authors (id, name) VALUES (1, 'test1');
INSERT INTO public.authors (id, name) VALUES (4, 'test2');
SELECT pg_catalog.setval('public.articles_author_id_seq', 1, false);
SELECT pg_catalog.setval('public.articles_id_seq', 1, false);
SELECT pg_catalog.setval('public.authors_id_seq', 1, false);
`,
			false,
			func(t *testing.T) {
				c := testutil.NewHttpcClient(t, port, nil)
				q := v1query.New(c, "v2/query")
				b, err := ioutil.ReadFile("testdata/seeds/articles.sql")
				require.NoError(t, err)
				_, err = q.PGRunSQL(hasura.PGRunSQLInput{
					SQL: string(b),
				})
				require.NoError(t, err)
				b, err = ioutil.ReadFile("testdata/seeds/authors.sql")
				require.NoError(t, err)
				_, err = q.PGRunSQL(hasura.PGRunSQLInput{
					SQL: string(b),
				})
				require.NoError(t, err)
			},
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
			got, err := d.ExportDatadump(tt.args.tableNames, tt.args.sourceName)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)
			require.Equal(t, tt.want, string(gotb))
		})
	}
}
