package version

import (
	"context"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type serverVersionResponse struct {
	Version string `json:"version"`
}

// FetchServerVersion reads the version from server.
func FetchServerVersion(endpoint string, client *httpc.Client) (version string, err error) {
	var op errors.Op = "version.FetchServerVersion"

	// Build the request through the shared httpc client so that any custom headers
	// configured on it (e.g. a --header "x-api-key: ..." auth header) are applied to the
	// version probe too. http.NewRequest would bypass the client's headers.
	req, err := client.NewRequest(http.MethodGet, endpoint, nil)
	if err != nil {
		return "", errors.E(op, fmt.Errorf("error while making http request to %s", endpoint))
	}

	var v serverVersionResponse

	response, err := client.Do(context.Background(), req, &v)
	if err != nil {
		return "", errors.E(op, fmt.Errorf("failed making version api call: %w", err))
	}

	if response.StatusCode != http.StatusOK {
		switch response.StatusCode {
		case http.StatusNotFound:
			return "", nil
		default:
			return "", errors.E(
				op,
				errors.KindHasuraAPI,
				fmt.Errorf("GET %s failed - [%d]", endpoint, response.StatusCode),
			)
		}
	}

	return v.Version, nil
}
