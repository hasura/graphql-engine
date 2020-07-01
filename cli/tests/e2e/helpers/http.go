package helpers

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
)

var (
	testHasuraCLIAPIUrl = "http://localhost:9693/apis"
)

func SendToHasuraMigrateAPIBodyFromFile(filepath string) (*http.Response, error) {
	b, err := ioutil.ReadFile(filepath)
	if err != nil {
		return nil, err
	}
	var body map[string]interface{}
	err = json.Unmarshal(b, &body)
	if err != nil {
		return nil, err
	}

	req, err := NewRequest("POST", fmt.Sprintf("%s/%s", testHasuraCLIAPIUrl, "migrate"), body)
	if err != nil {
		return nil, err
	}

	c := http.Client{}
	resp, err := c.Do(req)
	if err != nil {
		return nil, err
	}

	return resp, nil
}

func NewRequest(method, urlStr string, body interface{}) (*http.Request, error) {
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
