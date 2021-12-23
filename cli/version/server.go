package version

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

type serverVersionResponse struct {
	Version string `json:"version"`
}

// FetchServerVersion reads the version from server.
func FetchServerVersion(endpoint string, client *http.Client) (version string, err error) {
	response, err := client.Get(endpoint)
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
	} else {
		defer response.Body.Close()
		data, err := ioutil.ReadAll(response.Body)
		if err != nil {
			return "", fmt.Errorf("cannot read version api response: %w", err)
		}
		var v serverVersionResponse
		err = json.Unmarshal(data, &v)
		if err != nil {
			return "", fmt.Errorf("failed to parse version api response: %w", err)
		}
		return v.Version, nil
	}
}
