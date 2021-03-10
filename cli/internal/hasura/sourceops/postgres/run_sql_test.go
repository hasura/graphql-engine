package postgres

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/internal/testutil"

	"github.com/hasura/graphql-engine/cli/internal/hasura"
	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

func TestHasuraDatabaseOperations_RunSQL(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraVersion)
	defer teardown()
	type fields struct {
		httpClient *httpc.Client
		path       string
	}
	type args struct {
		input    hasura.PGRunSQLInput
		database string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *hasura.PGRunSQLOutput
		wantErr bool
	}{
		{
			"can send a run_sql request",
			fields{
				httpClient: testutil.NewHttpcClient(t, port, nil),
				path:       "v1/query",
			},
			args{
				input: hasura.PGRunSQLInput{
					SQL: "CREATE TABLE users();",
				},
			},
			&hasura.PGRunSQLOutput{
				ResultType: hasura.CommandOK,
				Result:     nil,
			},
			false,
		},
		{
			"can send a run_sql request",
			fields{
				httpClient: testutil.NewHttpcClient(t, port, nil),
				path:       "v2/query",
			},
			args{
				input: hasura.PGRunSQLInput{
					SQL:    "CREATE TABLE users2();",
					Source: "default",
				},
			},
			&hasura.PGRunSQLOutput{
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
				got, err := h.PGRunSQL(tt.args.input)
				if (err != nil) != tt.wantErr {
					t.Errorf("RunSQL() error = %v, wantErr %v", err, tt.wantErr)
					return
				}
				assert.Equal(t, tt.want, got)
			}
			test()
		})
	}
}
