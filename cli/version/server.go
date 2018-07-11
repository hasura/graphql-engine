package version

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"

	yaml "github.com/ghodss/yaml"
	"github.com/pkg/errors"
)

type serverVersionResponse struct {
	Version string `json:"version"`
}

// FetchServerVersion reads the version from server.
func FetchServerVersion(endpoint string) (version string, err error) {
	ep, err := url.Parse(endpoint)
	if err != nil {
		return "", errors.Wrap(err, "cannot parse endpoint as a valid url")
	}
	versionEndpoint := fmt.Sprintf("%s/v1/version", ep.String())
	response, err := http.Get(versionEndpoint)
	if err != nil {
		return "", errors.Wrap(err, "failed making version api call")
	}
	if response.StatusCode != http.StatusOK {
		switch response.StatusCode {
		case http.StatusNotFound:
			return "", nil
		default:
			return "", errors.Errorf("GET %s failed - [%d]", versionEndpoint, response.StatusCode)
		}
	} else {
		defer response.Body.Close()
		data, err := ioutil.ReadAll(response.Body)
		if err != nil {
			return "", errors.Wrap(err, "cannot read version api response")
		}
		var v serverVersionResponse
		err = yaml.Unmarshal(data, &v)
		if err != nil {
			return "", errors.Wrap(err, "failed to parse version api response")
		}
		return v.Version, nil
	}
}
