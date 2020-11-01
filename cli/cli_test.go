package cli

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/version"
)

func TestServerAPIPaths_SetDefaults(t *testing.T) {
	type fields struct {
		Query    string
		Metadata string
		GraphQL  string
		Config   string
		PGDump   string
		Version  string
	}
	type args struct {
		serverFeatureFlags version.ServerFeatureFlags
	}
	type want struct {
		query    string
		metadata string
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   want
	}{
		{
			"sets v2/query and v1/metadata for multiple datasources",
			fields{
				Query:    "",
				Metadata: "",
			},
			args{
				serverFeatureFlags: version.ServerFeatureFlags{
					HasDatasources: true,
				},
			},
			want{
				query:    "v2/query",
				metadata: "v1/metadata",
			},
		},
		{
			"sets v1/query for pre multiple datasource",
			fields{
				Query:    "",
				Metadata: "",
			},
			args{
				serverFeatureFlags: version.ServerFeatureFlags{
					HasDatasources: false,
				},
			},
			want{
				query:    "v1/query",
				metadata: "",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := &ServerAPIPaths{
				Query:    tt.fields.Query,
				Metadata: tt.fields.Metadata,
				GraphQL:  tt.fields.GraphQL,
				Config:   tt.fields.Config,
				PGDump:   tt.fields.PGDump,
				Version:  tt.fields.Version,
			}
			s.SetDefaults(&tt.args.serverFeatureFlags)
			assert.Equal(t, tt.want.query, s.Query)
			assert.Equal(t, tt.want.metadata, s.Metadata)
		})
	}
}
