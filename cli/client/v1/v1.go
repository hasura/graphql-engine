package v1

import (
	"fmt"
	"net/url"
	"path"
	"strings"
)

const (
	schemaMetadataAPIEndpoint = "v1/query"
	pgDumpAPIEndpoint         = "v1alpha1/pg_dump"
)

var (
	ErrNilConfig      = fmt.Errorf("no config")
	ErrNoDatabaseName = fmt.Errorf("no database name")
	ErrNoSchema       = fmt.Errorf("no schema")
	ErrDatabaseDirty  = fmt.Errorf("database is dirty")
)

// Client will implement the hasura V1 API
type Client struct {
	ServerBaseURL             url.URL
	SchemaMetadataAPIEndpoint url.URL
	PGDumpAPIEndpoint         url.URL
	Headers                   map[string]string
}

// NewClient when provided a URL will return a API Client with default config
func NewClient(hasuraAPIEndpoint string, headers map[string]string) (*Client, error) {
	parsedHasuraAPIEndpoint, parsedHeaders, err := ParseHasuraAPIEndpoint(hasuraAPIEndpoint)
	if err != nil {
		return nil, err
	}
	client := &Client{
		ServerBaseURL: *parsedHasuraAPIEndpoint,
		Headers:       make(map[string]string),
	}
	client.SchemaMetadataAPIEndpoint = url.URL{
		Path:   path.Join(parsedHasuraAPIEndpoint.Path, schemaMetadataAPIEndpoint),
		Host:   parsedHasuraAPIEndpoint.Host,
		Scheme: parsedHasuraAPIEndpoint.Scheme,
	}

	client.PGDumpAPIEndpoint = url.URL{
		Path:   path.Join(parsedHasuraAPIEndpoint.Path, pgDumpAPIEndpoint),
		Host:   parsedHasuraAPIEndpoint.Host,
		Scheme: parsedHasuraAPIEndpoint.Scheme,
	}

	// Add headers to
	for key, value := range headers {
		client.Headers[key] = value
	}
	for key, value := range parsedHeaders {
		client.Headers[key] = value
	}
	return client, nil
}

// ParseHasuraAPIEndpoint and return the parsed URL and Headers (extracted from query strings)
func ParseHasuraAPIEndpoint(hasuraAPIEndpoint string) (*url.URL, map[string]string, error) {
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
