package pgdump

import (
	"io/ioutil"
	"testing"

	pg "github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

func TestClient_Send(t *testing.T) {
	portHasuraV13, teardown13 := testutil.StartHasura(t, "hasura/graphql-engine:v1.3.3")
	defer teardown13()
	portHasuraLatest, teardownLatest := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardownLatest()
	type fields struct {
		Client *httpc.Client
		path   string
	}
	type args struct {
		request hasura.PGDumpRequest
	}

	pgclient := pg.New(testutil.NewHttpcClient(t, portHasuraV13, nil), "v1/query")
	sqlInput := hasura.PGRunSQLInput{
		SQL: `CREATE TABLE test (
   section NUMERIC NOT NULL,
   id1     NUMERIC NOT NULL,
   id2     NUMERIC NOT NULL
);`,
	}
	_, err := pgclient.PGRunSQL(sqlInput)
	require.NoError(t, err)
	pgclient = pg.New(testutil.NewHttpcClient(t, portHasuraLatest, nil), "v2/query")
	_, err = pgclient.PGRunSQL(sqlInput)
	require.NoError(t, err)

	tests := []struct {
		name      string
		fields    fields
		args      args
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can make a pg_dump hasura/graphql-engine:v1.3.3",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraV13, nil),
				path:   "/v1alpha1/pg_dump",
			},
			args{
				request: hasura.PGDumpRequest{
					Opts:        []string{"--schema-only", "--table", "test"},
					CleanOutput: true,
				},
			},
			`CREATE TABLE public.test (
    section numeric NOT NULL,
    id1 numeric NOT NULL,
    id2 numeric NOT NULL
);
ALTER TABLE public.test OWNER TO test;
`,
			false,
			require.NoError,
		},
		{
			"can make a pg_dump on latest",
			fields{
				Client: testutil.NewHttpcClient(t, portHasuraLatest, nil),
				path:   "/v1alpha1/pg_dump",
			},
			args{
				request: hasura.PGDumpRequest{
					Opts:        []string{"--schema-only", "--table", "test"},
					CleanOutput: true,
				},
			},
			`SET check_function_bodies = false;
CREATE TABLE public.test (
    section numeric NOT NULL,
    id1 numeric NOT NULL,
    id2 numeric NOT NULL
);
ALTER TABLE public.test OWNER TO test;
`,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &Client{
				Client: tt.fields.Client,
				path:   tt.fields.path,
			}
			got, err := c.Send(tt.args.request)
			tt.assertErr(t, err)
			if !tt.wantErr {
				gotb, err := ioutil.ReadAll(got)
				require.NoError(t, err)
				require.Equal(t, tt.want, string(gotb))
			}
		})
	}
}
