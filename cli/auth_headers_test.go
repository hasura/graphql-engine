package cli

import (
	"context"
	"net/http"
	"net/http/httptest"
	"sync"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newTestVersion(hasAccessKey bool) *version.Version {
	return &version.Version{
		ServerFeatureFlags: &version.ServerFeatureFlags{HasAccessKey: hasAccessKey},
	}
}

// TestAdminSecretRequestHeaders is the unit-level regression test for the empty
// admin-secret header bug: when no secret is configured, no admin-secret/access-key
// header must be produced at all (an empty-valued header would short-circuit server auth
// before a custom --header or the configured webhook/JWT is consulted).
func TestAdminSecretRequestHeaders(t *testing.T) {
	tests := []struct {
		name         string
		adminSecret  string
		adminSecrets []string
		hasAccessKey bool
		want         map[string]string
	}{
		{"no secret -> no header", "", nil, false, nil},
		{"admin secret -> admin secret header", "s3cr3t", nil, false, map[string]string{XHasuraAdminSecret: "s3cr3t"}},
		{"admin secrets list -> first as admin secret header", "", []string{"a", "b"}, false, map[string]string{XHasuraAdminSecret: "a"}},
		{"legacy access-key server + secret -> access key header", "s3cr3t", nil, true, map[string]string{XHasuraAccessKey: "s3cr3t"}},
		{"legacy access-key server, no secret -> still no header", "", nil, true, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ec := &ExecutionContext{
				Config: &Config{ServerConfig: ServerConfig{
					AdminSecret:  tt.adminSecret,
					AdminSecrets: tt.adminSecrets,
				}},
				Version: newTestVersion(tt.hasAccessKey),
			}
			got := ec.adminSecretRequestHeaders()
			if tt.want == nil {
				assert.Empty(t, got)
				return
			}
			assert.Equal(t, tt.want, got)
		})
	}
}

// TestValidateHeaderAssembly_Transport mirrors the header assembly performed in
// ExecutionContext.Validate (admin secret header, then custom --header, then SetHeaders)
// and drives real requests at an httptest server, asserting via header-map membership
// (not Header.Get, which cannot tell absent from empty):
//   - with no admin secret configured, NO X-Hasura-Admin-Secret / X-Hasura-Access-Key
//     header is sent on any authenticated request, while the custom header is intact;
//   - with a secret configured, the correct admin/access-key header is sent with its
//     value and the custom header is still intact.
func TestValidateHeaderAssembly_Transport(t *testing.T) {
	paths := []string{"/v1alpha1/config", "/v1/query", "/v1/metadata"}

	tests := []struct {
		name         string
		adminSecret  string
		hasAccessKey bool
		custom       []string
		wantAdminKey string // header name expected present, or "" if none must be sent
		wantAdminVal string
	}{
		{
			name:         "no secret + x-api-key: no admin header, api key intact",
			adminSecret:  "",
			hasAccessKey: false,
			custom:       []string{"X-Api-Key: myapikey"},
			wantAdminKey: "",
		},
		{
			name:         "configured secret + x-api-key: both sent",
			adminSecret:  "cfg-secret",
			hasAccessKey: false,
			custom:       []string{"X-Api-Key: myapikey"},
			wantAdminKey: XHasuraAdminSecret,
			wantAdminVal: "cfg-secret",
		},
		{
			name:         "legacy access-key server, no secret: no access-key header",
			adminSecret:  "",
			hasAccessKey: true,
			custom:       []string{"X-Api-Key: myapikey"},
			wantAdminKey: "",
		},
		{
			name:         "legacy access-key server + secret: access-key header sent",
			adminSecret:  "cfg-secret",
			hasAccessKey: true,
			custom:       []string{"X-Api-Key: myapikey"},
			wantAdminKey: XHasuraAccessKey,
			wantAdminVal: "cfg-secret",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var mu sync.Mutex
			captured := map[string]http.Header{}
			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				mu.Lock()
				captured[r.URL.Path] = r.Header.Clone()
				mu.Unlock()
				w.WriteHeader(http.StatusOK)
			}))
			defer srv.Close()

			headers, err := ParseHGEHeaders(tt.custom)
			require.NoError(t, err)

			ec := &ExecutionContext{
				Config:     &Config{ServerConfig: ServerConfig{AdminSecret: tt.adminSecret}},
				Version:    newTestVersion(tt.hasAccessKey),
				HGEHeaders: headers,
			}

			client, err := httpc.New(nil, srv.URL, ec.HGEHeaders)
			require.NoError(t, err)

			// Mirror ExecutionContext.Validate header assembly.
			ec.AddRequestHeaders(ec.adminSecretRequestHeaders())
			ec.AddRequestHeaders(ec.HGEHeaders)
			client.SetHeaders(ec.requestHeaders)

			for _, p := range paths {
				req, err := client.NewRequest(http.MethodGet, p, nil)
				require.NoError(t, err)
				_, err = client.Do(context.Background(), req, nil)
				require.NoError(t, err)
			}

			for _, p := range paths {
				h := captured[p]
				require.NotNilf(t, h, "no request captured for %s", p)

				apiVals, ok := h[http.CanonicalHeaderKey("X-Api-Key")]
				require.Truef(t, ok, "%s: x-api-key must be present", p)
				assert.Equalf(t, []string{"myapikey"}, apiVals, "%s: x-api-key intact", p)

				_, adminPresent := h[http.CanonicalHeaderKey(XHasuraAdminSecret)]
				_, accessPresent := h[http.CanonicalHeaderKey(XHasuraAccessKey)]

				if tt.wantAdminKey == "" {
					assert.Falsef(t, adminPresent,
						"%s: X-Hasura-Admin-Secret must be ABSENT (not empty) when no secret configured", p)
					assert.Falsef(t, accessPresent,
						"%s: X-Hasura-Access-Key must be ABSENT when no secret configured", p)
				} else {
					vals, ok := h[http.CanonicalHeaderKey(tt.wantAdminKey)]
					require.Truef(t, ok, "%s: %s must be present", p, tt.wantAdminKey)
					assert.Equalf(t, []string{tt.wantAdminVal}, vals, "%s: %s value", p, tt.wantAdminKey)
				}
			}
		})
	}
}
