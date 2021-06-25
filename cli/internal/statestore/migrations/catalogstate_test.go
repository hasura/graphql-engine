package migrations

import (
	"strconv"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/catalogstate"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
)

func TestCatalogStateStore_InsertVersion(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	defer teardown()
	type fields struct {
		c *statestore.CLICatalogState
	}
	type args struct {
		database string
		version  int64
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		{
			"can insert version into catalog state",
			fields{
				statestore.NewCLICatalogState(catalogstate.New(testutil.NewHttpcClient(t, port, nil), "v1/metadata"))},
			args{
				database: "test",
				version:  321312321321321,
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			m := &CatalogStateStore{
				c: tt.fields.c,
			}
			err := m.InsertVersion(tt.args.database, tt.args.version)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				state, err := m.getCLIState()
				assert.NoError(t, err)
				assert.Equal(t, map[string]bool{strconv.Itoa(int(tt.args.version)): false}, state.GetMigrationsByDatabase(tt.args.database))
			}
		})
	}
}
