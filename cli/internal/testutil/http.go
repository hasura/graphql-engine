package testutil

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/url"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func SendHTTPRequestWithFileAsBody(t *testing.T, filepath, url string) *http.Response {
	t.Helper()

	b, err := os.ReadFile(filepath)
	require.NoError(t, err)

	var body map[string]any

	err = json.Unmarshal(b, &body)
	require.NoError(t, err)

	req := NewRequest(t, "POST", url, body)

	c := http.Client{}
	resp, err := c.Do(req)
	require.NoError(t, err)

	return resp
}

func NewRequest(t *testing.T, method, urlStr string, body any) *http.Request {
	t.Helper()

	u, err := url.ParseRequestURI(urlStr)
	require.NoError(t, err)

	var buf io.ReadWriter
	if body != nil {
		buf = &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		err := enc.Encode(body)
		require.NoError(t, err)
	}

	req, err := http.NewRequest(method, u.String(), buf)
	require.NoError(t, err)

	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}

	if len(TestAdminSecret) > 0 {
		req.Header.Set("X-Hasura-Admin-Secret", TestAdminSecret)
	}

	return req
}
