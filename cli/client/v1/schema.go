package v1

import (
	"encoding/json"
	"net/http"

	"github.com/parnurzeal/gorequest"
)

const (
	TuplesOK  = "TuplesOk"
	CommandOK = "CommandOk"
)

// SendQuery does what the name implies
func (client *Client) SendQuery(m interface{}) (*http.Response, []byte, *Error) {
	request := gorequest.New()
	request = request.Post(client.SchemaMetadataAPIEndpoint.String()).Send(m)

	for headerName, headerValue := range client.Headers {
		request.Set(headerName, headerValue)
	}

	resp, body, errs := request.EndBytes()
	if len(errs) != 0 {
		return resp, body, E(errs[0])
	}

	if resp.StatusCode != http.StatusOK {
		var apiError APIError
		err := json.Unmarshal(body, &apiError)
		if err != nil {
			return nil, nil, E(err)
		}
		return nil, nil, E(apiError)
	}
	// TODO: Check if the body can always be an instance of HasuraSQLResponse
	return resp, body, nil
}

// TODO: Check if this naming makes sense
type QueryResponse struct {
	ResultType string     `json:"result_type"`
	Result     [][]string `json:"result"`
}

// TODO: Check if this naming makes sense
type SendBulkQueryPayload struct {
	Type string             `json:"type" yaml:"type"`
	Args []SendQueryPayload `json:"args" yaml:"args"`
}

type SendQueryPayload struct {
	Type string               `json:"type" yaml:"type"`
	Args SendQueryPayloadArgs `json:"args" yaml:"args"`
}

type SendQueryPayloadArgs struct {
	SQL       string        `json:"sql,omitempty" yaml:"sql"`
	Table     interface{}   `json:"table,omitempty"`
	Columns   interface{}   `json:"columns,omitempty"`
	Where     interface{}   `json:"where,omitempty"`
	OrderBy   interface{}   `json:"order_by,omitempty"`
	Objects   []interface{} `json:"objects,omitempty"`
	Limit     int           `json:"limit,omitempty"`
	Returning []string      `json:"returning,omitempty"`
	Set       interface{}   `json:"$set,omitempty"`
}
