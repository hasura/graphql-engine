package v1

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
)

type PGDumpService service

const pgDumpAPIEndpoint = "v1/alpha1/pgdump"

// SendPGDumpQuery --
func (s *PGDumpService) SendPGDumpQuery(payload interface{}) (*http.Response, []byte, *Error) {
	request, err := s.client.NewRequest("POST", pgDumpAPIEndpoint, payload)
	if err != nil {
		return nil, nil, E(err)
	}

	resp, err := s.client.Do(request)
	if err != nil {
		return nil, nil, E(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, nil, E(err)
	}
	if resp.StatusCode != http.StatusOK {
		var apiError APIError
		err := json.Unmarshal(body, &apiError)
		if err != nil {
			return nil, nil, E(err)
		}
		return nil, nil, E(apiError)
	}
	return resp, body, nil
}
