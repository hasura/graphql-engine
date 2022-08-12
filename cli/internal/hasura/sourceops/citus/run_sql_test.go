package citus

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

func TestCitus_RunSQL(t *testing.T) {
	port, sourceName, teardown := testutil.StartHasuraWithCitusSource(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		httpClient *httpc.Client
		path       string
	}
	type args struct {
		input hasura.CitusRunSQLInput
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *hasura.CitusRunSQLOutput
		wantErr bool
	}{
		{
			"can send a run_sql request",
			fields{
				httpClient: testutil.NewHttpcClient(t, port, nil),
				path:       "v2/query",
			},
			args{
				input: hasura.CitusRunSQLInput{
					SQL:    "CREATE TABLE users2();",
					Source: sourceName,
				},
			},
			&hasura.CitusRunSQLOutput{
				ResultType: hasura.CommandOK,
				Result:     nil,
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			test := func() {
				h := &SourceOps{
					Client: tt.fields.httpClient,
					path:   tt.fields.path,
				}
				got, err := h.CitusRunSQL(tt.args.input)
				if (err != nil) != tt.wantErr {
					t.Errorf("CitusRunSQL() error = %v, wantErr %v", err, tt.wantErr)
					return
				}
				assert.Equal(t, tt.want, got)
			}
			test()
		})
	}
}
