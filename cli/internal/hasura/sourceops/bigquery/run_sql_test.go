package bigquery

import (
	"fmt"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestHasuraDatabaseOperations_RunSQL(t *testing.T) {
	port, source, projectId, dataset, teardown := testutil.StartHasuraWithBigQuerySource(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		httpClient *httpc.Client
		path       string
	}
	type args struct {
		input hasura.BigQueryRunSQLInput
	}
	tests := []struct {
		name      string
		fields    fields
		args      args
		want      *hasura.BigQueryRunSQLOutput
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
				input: hasura.BigQueryRunSQLInput{
					SQL:    fmt.Sprintf("CREATE TABLE IF NOT EXISTS `%s.%s.test_1` (first_name STRING,last_name STRING);", projectId, dataset),
					Source: source,
				},
			},
			&hasura.BigQueryRunSQLOutput{
				ResultType: hasura.TuplesOK,
				Result:     [][]string{{}},
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
				got, err := h.BigQueryRunSQL(tt.args.input)
				tt.assertErr(t, err)
				if tt.wantErr {
					return
				}
				assert.Equal(t, tt.want, got)
			}
			test()
		})
	}
}
