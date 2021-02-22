package databaseops

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
		input    hasura.RunSQLInput
		database string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *hasura.RunSQLOutput
		wantErr bool
	}{
		{
			"can send a run_sql request",
			fields{
				httpClient: testutil.NewHttpcClient(t, port, nil),
				path:       "v1/query",
			},
			args{
				input: hasura.RunSQLInput{
					SQL: "CREATE TABLE users();",
				},
			},
			&hasura.RunSQLOutput{
				ResultType: hasura.CommandOK,
				Result:     nil,
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			test := func() {
				h := &ClientDatabaseOps{
					Client: tt.fields.httpClient,
					path:   tt.fields.path,
				}
				got, err := h.RunSQL(tt.args.input)
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
