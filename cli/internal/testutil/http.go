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

func SendHTTPRequestWithFileAsBody(t *testing.T, filepath, url string) (*http.Response, error) {
	b, err := ioutil.ReadFile(filepath)
	require.NoError(t, err)
	var body map[string]interface{}
	err = json.Unmarshal(b, &body)
	require.NoError(t, err)

	req, err := newPOSTRequest(t, "POST", url, body)
	require.NoError(t, err)

	c := http.Client{}
	resp, err := c.Do(req)
	require.NoError(t, err)

	return resp, nil
}

func newPOSTRequest(t *testing.T, method, urlStr string, body interface{}) (*http.Request, error) {
	u, err := url.ParseRequestURI(urlStr)
	if err != nil {
		return nil, err
	}
	var buf io.ReadWriter
	if body != nil {
		buf = &bytes.Buffer{}
		enc := json.NewEncoder(buf)
		enc.SetEscapeHTML(false)
		err := enc.Encode(body)
		if err != nil {
			return nil, err
		}
	}

	req, err := http.NewRequest(method, u.String(), buf)
	if err != nil {
		return nil, err
	}

	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	return req, nil
}
