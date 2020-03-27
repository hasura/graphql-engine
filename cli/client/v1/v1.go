package v1

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/url"
	"strings"

	"github.com/pkg/errors"
)

// Client will implement the hasura V1 API
type Client struct {
	client    *http.Client
	UserAgent string

	common  service
	BaseURL url.URL
	// Default headers are obtained by parsing the raw url string
	// or explicitly passing during initialisation
	// the explicitly passed headers will have precedence over the
	// ones passed in url
	defaultHeaders map[string]string
	*QueryService
	*PGDumpService
}

type service struct {
	client *Client
}

// NewClient when provided a URL will return a API Client with default config
func NewClient(hasuraAPIBaseURL string, httpClient *http.Client, headers map[string]string) (*Client, error) {
	if httpClient == nil {
		httpClient = &http.Client{}
	}
	parsedHasuraAPIEndpoint, parsedHeaders, err := ParseHasuraAPIBaseURL(hasuraAPIBaseURL)
	if err != nil {
		return nil, err
	}
	client := &Client{
		client:         httpClient,
		BaseURL:        *parsedHasuraAPIEndpoint,
		defaultHeaders: make(map[string]string),
	}
	for key, value := range parsedHeaders {
		client.defaultHeaders[key] = value
	}
	for key, value := range headers {
		client.defaultHeaders[key] = value
	}
	client.common.client = client
	client.QueryService = (*QueryService)(&client.common)
	return client, nil
}

func (c *Client) Do(req *http.Request) (*http.Response, error) {
	return c.client.Do(req)
}

// NewRequest creates an API request. A relative URL can be provided in urlStr,
// in which case it is resolved relative to the BaseURL of the Client.
// Relative URLs should always be specified without a preceding slash. If
// specified, the value pointed to by body is JSON encoded and included as the
// request body.
func (c *Client) NewRequest(method, urlStr string, body interface{}) (*http.Request, error) {
	u, err := c.BaseURL.Parse(urlStr)
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
			return nil, errors.Wrap(err, "error creating request")
		}
	}

	req, err := http.NewRequest(method, u.String(), buf)
	if err != nil {
		return nil, errors.Wrap(err, "error creating request")
	}

	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	for k, v := range c.defaultHeaders {
		req.Header.Set(k, v)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	return req, nil
}

// ParseHasuraAPIBaseURL and return the parsed URL and Headers (extracted from query strings)
func ParseHasuraAPIBaseURL(hasuraAPIEndpoint string) (*url.URL, map[string]string, error) {
	parsedHasuraAPIEndpoint, err := url.Parse(hasuraAPIEndpoint)
	if err != nil {
		return nil, nil, err
	}

	params := parsedHasuraAPIEndpoint.Query()
	headers := make(map[string]string)
	if queryHeaders, ok := params["headers"]; ok {
		for _, header := range queryHeaders {
			headerValue := strings.SplitN(header, ":", 2)
			if len(headerValue) == 2 && headerValue[1] != "" {
				headers[headerValue[0]] = headerValue[1]
			}
		}
	}

	// Use sslMode query param to set Scheme
	var scheme string
	sslMode := params.Get("sslmode")
	if sslMode == "enable" {
		scheme = "https"
	} else {
		scheme = "http"
	}
	parsedHasuraAPIEndpoint.Scheme = scheme

	return parsedHasuraAPIEndpoint, headers, nil
}
