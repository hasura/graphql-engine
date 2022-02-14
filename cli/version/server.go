package version

import (
	"context"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type serverVersionResponse struct {
	Version string `json:"version"`
}

// FetchServerVersion reads the version from server.
func FetchServerVersion(endpoint string, client *httpc.Client) (version string, err error) {
	req, err := http.NewRequest("GET", endpoint, nil)
	if err != nil {
		return "", fmt.Errorf("error while making http request to %s", endpoint)
	}
	var v serverVersionResponse
	response, err := client.Do(context.Background(), req, &v)
	if err != nil {
		return "", fmt.Errorf("failed making version api call: %w", err)
	}
	if response.StatusCode != http.StatusOK {
		switch response.StatusCode {
		case http.StatusNotFound:
			return "", nil
		default:
			return "", fmt.Errorf("GET %s failed - [%d]", endpoint, response.StatusCode)
		}
	}
	return v.Version, nil
}
