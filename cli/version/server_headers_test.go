package version

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestFetchServerVersion_ForwardsClientHeaders is the regression test for the
// version-probe header bypass: FetchServerVersion previously built its request with
// http.NewRequest, which skips the shared httpc client's headers, so a custom --header
// (e.g. x-api-key) never reached the /v1/version probe. It must now be forwarded.
func TestFetchServerVersion_ForwardsClientHeaders(t *testing.T) {
	var got http.Header
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		got = r.Header.Clone()
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte(`{"version":"v2.0.0"}`))
	}))
	defer srv.Close()

	client, err := httpc.New(nil, srv.URL, map[string]string{"X-Api-Key": "myapikey"})
	require.NoError(t, err)

	v, err := FetchServerVersion(srv.URL+"/v1/version", client)
	require.NoError(t, err)
	assert.Equal(t, "v2.0.0", v)

	vals, ok := got[http.CanonicalHeaderKey("X-Api-Key")]
	require.Truef(t, ok, "x-api-key header must be present on the version probe, got %v", got)
	assert.Equal(t, []string{"myapikey"}, vals)
}
