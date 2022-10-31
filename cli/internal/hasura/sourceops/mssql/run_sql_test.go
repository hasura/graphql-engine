package mssql

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

func TestHasuraDatabaseOperations_RunSQL(t *testing.T) {
	port, mssqlSourceName, teardown := testutil.StartHasuraWithMSSQLSource(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		httpClient *httpc.Client
		path       string
	}
	type args struct {
		input hasura.MSSQLRunSQLInput
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		want      *hasura.MSSQLRunSQLOutput
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can send a run_sql request",
			fields{
				httpClient: testutil.NewHttpcClient(t, port, nil),
				path:       "v2/query",
			},
			args{
				input: hasura.MSSQLRunSQLInput{
					SQL:    "select 1",
					Source: mssqlSourceName,
				},
			},
			&hasura.MSSQLRunSQLOutput{
				ResultType: hasura.TuplesOK,
				Result: [][]interface{}{
					{
						"",
					},
					{
						float64(1),
					},
				},
			},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			test := func() {
				h := &SourceOps{
					Client: tt.fields.httpClient,
					path:   tt.fields.path,
				}
				got, err := h.MSSQLRunSQL(tt.args.input)
				tt.assertErr(t, err)
				if !tt.wantErr {
					assert.Equal(t, tt.want, got)
				}
			}
			test()
		})
	}
}
