package cli

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestServerConfig_ParseEndpoint(t *testing.T) {
	tests := []struct {
		name     string
		endpoint string
		wantErr  string
	}{
		{"root url", "http://localhost:8080", ""},
		{"root url behind a path prefix", "https://example.com/hasura/", ""},
		{"path containing graphql", "https://example.com/graphql-engine", ""},
		{"graphql api url", "http://localhost:8080/v1/graphql", `use "http://localhost:8080" instead`},
		{"graphql api url with trailing slash", "https://example.com/hasura/v1/graphql/", `use "https://example.com/hasura" instead`},
		{"invalid url", "not a url", "invalid"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := &ServerConfig{Endpoint: tc.endpoint}
			err := c.ParseEndpoint()
			if tc.wantErr == "" {
				require.NoError(t, err)
				assert.Equal(t, tc.endpoint, c.ParsedEndpoint.String())
				return
			}
			require.Error(t, err)
			assert.Contains(t, err.Error(), tc.wantErr)
			assert.Nil(t, c.ParsedEndpoint)
		})
	}
}
