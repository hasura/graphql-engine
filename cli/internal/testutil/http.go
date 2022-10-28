package testutil

import (
	"bytes"
	"encoding/json"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"testing"

	"github.com/stretchr/testify/require"
)

func SendHTTPRequestWithFileAsBody(t *testing.T, filepath, url string) *http.Response {
	b, err := ioutil.ReadFile(filepath)
	require.NoError(t, err)
	var body map[string]interface{}
	err = json.Unmarshal(b, &body)
	require.NoError(t, err)

	req := NewRequest(t, "POST", url, body)

	c := http.Client{}
	resp, err := c.Do(req)
	require.NoError(t, err)

	return resp
}

func NewRequest(t *testing.T, method, urlStr string, body interface{}) *http.Request {
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
		req.Header.Set("x-hasura-admin-secret", TestAdminSecret)
	}
	return req
}
