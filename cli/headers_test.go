package cli

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParseHGEHeaders(t *testing.T) {
	tests := []struct {
		name    string
		input   []string
		want    map[string]string
		wantErr bool
		// errMustNotContain lists substrings (typically secret values) that must never
		// appear in the returned error message.
		errMustNotContain []string
	}{
		{
			name:  "nil input returns nil map",
			input: nil,
			want:  nil,
		},
		{
			name:  "empty input returns nil map",
			input: []string{},
			want:  nil,
		},
		{
			name:  "simple header",
			input: []string{"X-Custom: value"},
			want:  map[string]string{"X-Custom": "value"},
		},
		{
			name:  "authorization bearer token with colons and dots preserved",
			input: []string{"Authorization: Bearer abc.def.ghi"},
			want:  map[string]string{"Authorization": "Bearer abc.def.ghi"},
		},
		{
			name:  "value containing colons is preserved (only first colon splits)",
			input: []string{"X-Url: https://example.com:8080/path"},
			want:  map[string]string{"X-Url": "https://example.com:8080/path"},
		},
		{
			name:  "value containing commas is preserved",
			input: []string{"X-List: a,b,c"},
			want:  map[string]string{"X-List": "a,b,c"},
		},
		{
			name:  "surrounding whitespace around value is trimmed",
			input: []string{"X-Custom:    spaced-value   "},
			want:  map[string]string{"X-Custom": "spaced-value"},
		},
		{
			name:  "no space after colon still parses",
			input: []string{"X-Custom:value"},
			want:  map[string]string{"X-Custom": "value"},
		},
		{
			name:  "multiple distinct headers",
			input: []string{"Authorization: Bearer t", "X-Tenant: acme"},
			want: map[string]string{
				"Authorization": "Bearer t",
				"X-Tenant":      "acme",
			},
		},
		{
			name:    "missing colon is rejected and does not leak the raw input",
			input:   []string{"supersecretjwtvalue"},
			wantErr: true,
			// The raw input (which could be a pasted secret) must not be echoed.
			errMustNotContain: []string{"supersecretjwtvalue"},
		},
		{
			name:    "empty header name is rejected",
			input:   []string{": value"},
			wantErr: true,
		},
		{
			name:    "invalid header name characters are rejected",
			input:   []string{"Invalid Name: value"},
			wantErr: true,
		},
		{
			name:    "empty value is rejected",
			input:   []string{"X-Custom:   "},
			wantErr: true,
		},
		{
			name:              "CR in value is rejected (header injection) and value not leaked",
			input:             []string{"X-Custom: val\rInjected: evil"},
			wantErr:           true,
			errMustNotContain: []string{"Injected", "evil"},
		},
		{
			name:              "LF in value is rejected (header injection) and value not leaked",
			input:             []string{"X-Custom: val\nInjected: evil"},
			wantErr:           true,
			errMustNotContain: []string{"Injected", "evil"},
		},
		{
			name:    "trailing CRLF in value is rejected, not silently trimmed",
			input:   []string{"Authorization: Bearer token\r\n"},
			wantErr: true,
			// The trailing \r\n must not be stripped away, and the token must not leak.
			errMustNotContain: []string{"Bearer token"},
		},
		{
			name:              "leading CRLF in value is rejected",
			input:             []string{"Authorization: \r\nBearer token"},
			wantErr:           true,
			errMustNotContain: []string{"Bearer token"},
		},
		{
			name:              "trailing CR only in value is rejected",
			input:             []string{"X-Custom: secretval\r"},
			wantErr:           true,
			errMustNotContain: []string{"secretval"},
		},
		{
			name:              "trailing LF only in value is rejected",
			input:             []string{"X-Custom: secretval\n"},
			wantErr:           true,
			errMustNotContain: []string{"secretval"},
		},
		{
			name:    "trailing vertical tab (control char) in value is rejected",
			input:   []string{"X-Custom: value\v"},
			wantErr: true,
		},
		{
			name:    "trailing form feed (control char) in value is rejected",
			input:   []string{"X-Custom: value\f"},
			wantErr: true,
		},
		{
			name:    "trailing CR in header name is rejected",
			input:   []string{"X-Custom\r: value"},
			wantErr: true,
		},
		{
			name:    "leading LF in header name is rejected",
			input:   []string{"\nX-Custom: value"},
			wantErr: true,
		},
		{
			name:    "duplicate header names (exact) are rejected",
			input:   []string{"X-Custom: secret-one", "X-Custom: secret-two"},
			wantErr: true,
			// Values must not be surfaced in the duplicate error.
			errMustNotContain: []string{"secret-one", "secret-two"},
		},
		{
			name:              "duplicate header names (case-insensitive) are rejected",
			input:             []string{"Authorization: Bearer one", "authorization: Bearer two"},
			wantErr:           true,
			errMustNotContain: []string{"Bearer one", "Bearer two"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseHGEHeaders(tt.input)
			if tt.wantErr {
				require.Error(t, err)
				for _, s := range tt.errMustNotContain {
					assert.NotContains(t, err.Error(), s,
						"error message must not leak sensitive values")
				}

				return
			}

			require.NoError(t, err)
			assert.Equal(t, tt.want, got)
		})
	}
}

// TestHGEHeaders_PreflightPropagation verifies custom headers set at client
// construction time (as Validate does via httpc.New) are sent on requests, which
// covers the preflight version/status/config probes that run before the admin secret
// header is applied.
func TestHGEHeaders_PreflightPropagation(t *testing.T) {
	var received http.Header

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		received = r.Header.Clone()
		w.WriteHeader(http.StatusOK)
	}))
	defer server.Close()

	headers, err := ParseHGEHeaders([]string{"Authorization: Bearer jwt-token", "X-Tenant: acme"})
	require.NoError(t, err)

	client, err := httpc.New(nil, server.URL, headers)
	require.NoError(t, err)

	req, err := client.NewRequest(http.MethodGet, "/", nil)
	require.NoError(t, err)

	_, err = client.Do(context.Background(), req, nil)
	require.NoError(t, err)

	assert.Equal(t, "Bearer jwt-token", received.Get("Authorization"))
	assert.Equal(t, "acme", received.Get("X-Tenant"))
}

// TestHGEHeaders_MergeWithAdminSecret replicates the header merge performed in
// ExecutionContext.Validate and asserts that:
//   - the admin secret header and custom headers are both sent,
//   - custom headers survive the SetHeaders call (which replaces the header set),
//   - a custom header takes precedence over the admin secret header of the same name.
func TestHGEHeaders_MergeWithAdminSecret(t *testing.T) {
	adminSecretHeader := XHasuraAdminSecret

	t.Run("admin secret and custom headers both sent", func(t *testing.T) {
		var received http.Header
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			received = r.Header.Clone()
			w.WriteHeader(http.StatusOK)
		}))
		defer server.Close()

		headers, err := ParseHGEHeaders([]string{"X-Tenant: acme"})
		require.NoError(t, err)

		ec := &ExecutionContext{HGEHeaders: headers}
		client, err := httpc.New(nil, server.URL, ec.HGEHeaders)
		require.NoError(t, err)

		// Mirror Validate: admin secret first, then custom headers, then SetHeaders.
		ec.AddRequestHeaders(map[string]string{adminSecretHeader: "my-admin-secret"})
		ec.AddRequestHeaders(ec.HGEHeaders)
		client.SetHeaders(ec.requestHeaders)

		req, err := client.NewRequest(http.MethodGet, "/", nil)
		require.NoError(t, err)
		_, err = client.Do(context.Background(), req, nil)
		require.NoError(t, err)

		assert.Equal(t, "my-admin-secret", received.Get(adminSecretHeader))
		assert.Equal(t, "acme", received.Get("X-Tenant"))
	})

	t.Run("custom header overrides admin secret header of same name", func(t *testing.T) {
		var received http.Header
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			received = r.Header.Clone()
			w.WriteHeader(http.StatusOK)
		}))
		defer server.Close()

		headers, err := ParseHGEHeaders([]string{XHasuraAdminSecret + ": from-flag"})
		require.NoError(t, err)

		ec := &ExecutionContext{HGEHeaders: headers}
		client, err := httpc.New(nil, server.URL, ec.HGEHeaders)
		require.NoError(t, err)

		ec.AddRequestHeaders(map[string]string{adminSecretHeader: "from-config"})
		ec.AddRequestHeaders(ec.HGEHeaders)
		client.SetHeaders(ec.requestHeaders)

		req, err := client.NewRequest(http.MethodGet, "/", nil)
		require.NoError(t, err)
		_, err = client.Do(context.Background(), req, nil)
		require.NoError(t, err)

		assert.Equal(t, "from-flag", received.Get(adminSecretHeader))
	})

	// This is the regression test for the case-insensitive precedence bug: the admin
	// secret header is added in its canonical casing while the custom --header uses a
	// different casing for the SAME HTTP header. Without canonicalization in
	// AddRequestHeaders the merged map held two keys and the value sent depended on map
	// iteration order. The server must receive exactly ONE value for the header and it
	// must always be the custom one. Repeated many times to catch nondeterminism.
	t.Run("different-case custom header deterministically overrides admin secret", func(t *testing.T) {
		// A single fixed handler (not reassigned while the server runs) publishes each
		// request's headers to a buffered channel, so capture is race-free under -race.
		receivedCh := make(chan http.Header, 1)
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			receivedCh <- r.Header.Clone()
			w.WriteHeader(http.StatusOK)
		}))
		defer server.Close()

		// Custom flag uses a different casing than the canonical admin-secret header.
		differentCase := strings.ToLower(XHasuraAdminSecret) // "x-hasura-admin-secret"
		headers, err := ParseHGEHeaders([]string{differentCase + ": from-flag"})
		require.NoError(t, err)

		for i := range 50 {
			ec := &ExecutionContext{HGEHeaders: headers}
			client, err := httpc.New(nil, server.URL, ec.HGEHeaders)
			require.NoError(t, err)

			// Mirror Validate ordering: admin secret (canonical) first, then custom.
			ec.AddRequestHeaders(map[string]string{adminSecretHeader: "from-config"})
			ec.AddRequestHeaders(ec.HGEHeaders)
			client.SetHeaders(ec.requestHeaders)

			req, err := client.NewRequest(http.MethodGet, "/", nil)
			require.NoError(t, err)
			_, err = client.Do(context.Background(), req, nil)
			require.NoError(t, err)

			received := <-receivedCh

			// Exactly one value for the (canonical) header, and it is the custom one.
			values := received.Values(adminSecretHeader)
			require.Lenf(t, values, 1, "iteration %d: expected exactly one header value, got %v", i, values)
			require.Equalf(t, "from-flag", values[0],
				"iteration %d: custom --header must always win", i)
		}
	})

	t.Run("custom header only, no admin secret configured", func(t *testing.T) {
		var received http.Header
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			received = r.Header.Clone()
			w.WriteHeader(http.StatusOK)
		}))
		defer server.Close()

		headers, err := ParseHGEHeaders([]string{"Authorization: Bearer jwt-token"})
		require.NoError(t, err)

		ec := &ExecutionContext{HGEHeaders: headers}
		client, err := httpc.New(nil, server.URL, ec.HGEHeaders)
		require.NoError(t, err)

		// GetAdminSecret() returns "" when no admin secret is configured; Validate still
		// sets the (empty) admin secret header. The custom auth header must survive and
		// be sent so the request can authenticate without an admin secret.
		ec.AddRequestHeaders(map[string]string{adminSecretHeader: ""})
		ec.AddRequestHeaders(ec.HGEHeaders)
		client.SetHeaders(ec.requestHeaders)

		req, err := client.NewRequest(http.MethodGet, "/", nil)
		require.NoError(t, err)
		_, err = client.Do(context.Background(), req, nil)
		require.NoError(t, err)

		assert.Equal(t, "Bearer jwt-token", received.Get("Authorization"))
	})
}

// TestAddRequestHeaders_CanonicalizesAndOverrides verifies that AddRequestHeaders keys
// headers by their canonical name, so that a later header differing only in case
// overrides the earlier one instead of leaving two distinct map keys.
func TestAddRequestHeaders_CanonicalizesAndOverrides(t *testing.T) {
	ec := &ExecutionContext{}

	// Add the admin-secret header in canonical casing, then a custom header with the
	// same name in a different casing.
	ec.AddRequestHeaders(map[string]string{XHasuraAdminSecret: "from-config"})
	ec.AddRequestHeaders(map[string]string{strings.ToLower(XHasuraAdminSecret): "from-flag"})
	ec.AddRequestHeaders(map[string]string{"x-tenant-id": "acme"})

	// Only one key for the admin secret header (canonical), holding the custom value.
	_, lowerExists := ec.requestHeaders[strings.ToLower(XHasuraAdminSecret)]
	assert.False(t, lowerExists, "lower-cased duplicate key must not be present")
	assert.Equal(t, "from-flag", ec.requestHeaders[XHasuraAdminSecret])

	// Unrelated headers are preserved (stored under their canonical name).
	assert.Equal(t, "acme", ec.requestHeaders[http.CanonicalHeaderKey("x-tenant-id")])
	assert.Len(t, ec.requestHeaders, 2)
}

// TestHGEHeaders_CaseInsensitiveMergeTransport exercises the real readConfig merge of a
// case-differing programmatic header and a --header flag, then builds the preflight
// client exactly as Validate does (httpc.New with ec.HGEHeaders) and asserts the server
// receives the flag value exactly once. This is the regression test for the residual
// case-insensitive merge in readConfig: without dropping the case-variant programmatic
// key, two keys would reach httpc.New and the value sent would depend on map iteration
// order.
func TestHGEHeaders_CaseInsensitiveMergeTransport(t *testing.T) {
	fs := afero.NewMemMapFs()
	require.NoError(t, fs.MkdirAll("/project", 0o755))
	require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0o644))

	receivedCh := make(chan http.Header, 1)
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		receivedCh <- r.Header.Clone()
		w.WriteHeader(http.StatusOK)
	}))
	defer server.Close()

	for i := range 50 {
		v := viper.New()
		v.SetFs(fs)
		v.AddConfigPath("/project")
		ec := &ExecutionContext{
			Viper:          v,
			HGEHeaders:     map[string]string{"authorization": "programmatic"},
			HGEHeaderFlags: []string{"Authorization: Bearer from-flag"},
		}
		require.NoError(t, ec.readConfig())

		// Exactly one key remains after the case-insensitive merge.
		require.Lenf(t, ec.HGEHeaders, 1, "iteration %d: merged map should hold one key, got %v",
			i, ec.HGEHeaders)

		// Build the preflight client the same way Validate does.
		client, err := httpc.New(nil, server.URL, ec.HGEHeaders)
		require.NoError(t, err)

		req, err := client.NewRequest(http.MethodGet, "/", nil)
		require.NoError(t, err)
		_, err = client.Do(context.Background(), req, nil)
		require.NoError(t, err)

		received := <-receivedCh
		values := received.Values("Authorization")
		require.Lenf(t, values, 1, "iteration %d: expected exactly one Authorization value, got %v", i, values)
		require.Equalf(t, "Bearer from-flag", values[0],
			"iteration %d: flag value must win on initial httpc.New", i)
	}
}
