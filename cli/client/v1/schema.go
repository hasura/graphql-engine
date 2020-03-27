package v1

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"
)

const schemaMetadataAPIEndpoint = "v1/query"

func makePayloadValidationErr(requestType, reason string) error {
	return fmt.Errorf("validating payload for operation %s failed: %s", requestType, reason)
}

//Query request types: https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/index.html#request-types
const RunSQL = "run_sql"

type RunSQLPayload struct {
	Type string     `json:"type" yaml:"type"`
	Args RunSQLArgs `json:"args" yaml:"args"`
}

type RunSQLArgs struct {
	SQL                      string
	Cascade                  bool
	CheckMetadataConsistency bool
	ReadOnly                 bool
}

func (p *RunSQLPayload) Validate() error {
	if p.Type != RunSQL {
		return makePayloadValidationErr(RunSQL, fmt.Sprintf("bad request type provided: %v", p.Type))
	}
	return nil
}

const AddExistingTableOrView = "add_existing_table_or_view"

type AddExistingTableOrViewPayload struct {
	Type string                     `json:"type"`
	Args AddExistingTableOrViewArgs `json:"args"`
}
type AddExistingTableOrViewArgs struct {
	Name   string `json:"name"`
	Schema string `json:"schema"`
}

func (p *AddExistingTableOrViewPayload) Validate() error {
	if p.Type != AddExistingTableOrView {
		return makePayloadValidationErr(RunSQL, fmt.Sprintf("bad request type provided: %v", p.Type))
	}
	return nil
}

const Bulk = "bulk"

type BulkPayload struct {
	Type string
	Args []sendPayload
}

func (b *BulkPayload) Validate() error {
	for _, payload := range b.Args {
		if err := payload.Validate(); err != nil {
			return err
		}
	}
	return nil
}

// QueryService will implement the https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/index.html#schema-metadata-api-reference
type QueryService service

type sendPayload interface {
	Validate() error
}

func (s *QueryService) Send(payload sendPayload) (*http.Response, []byte, *Error) {
	if err := payload.Validate(); err != nil {
		return nil, nil, E(err)
	}
	request, err := s.client.NewRequest("POST", schemaMetadataAPIEndpoint, payload)
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

// BulkPayloadMaker will accept an list of json []byte and make a BulkPayload
// out of it by finding and assigning the correct underlying types to the interface
func BulkPayloadMaker(payloads ...[]byte) (*BulkPayload, error) {
	var bulkPayload = new(BulkPayload)
	bulkPayload.Type = Bulk

	for _, payload := range payloads {
		// decode the incoming bytes to map[string]interface{}
		// this is an intermediate step, which will find the correct type
		// and later decode it to the correct struct
		var intermediateDecode = map[string]interface{}{}
		err := json.Unmarshal(payload, intermediateDecode)
		if err != nil {
			return nil, errors.Wrap(err, "failed to decode payload")
		}
		requestType, ok := intermediateDecode["type"]
		if !ok {
			return nil, fmt.Errorf("request type not found in payload %s", string(payload))
		}
		switch requestType {
		case RunSQL:
			var runSQLPayload = new(RunSQLPayload)
			runSQLPayload.Type = RunSQL
			mapstructure.Decode(intermediateDecode, runSQLPayload)
			bulkPayload.Args = append(bulkPayload.Args, runSQLPayload)
		case AddExistingTableOrView:
			var addExistingTableOrViewPayload = new(AddExistingTableOrViewPayload)
			addExistingTableOrViewPayload.Type = AddExistingTableOrView
			mapstructure.Decode(intermediateDecode, addExistingTableOrViewPayload)
			bulkPayload.Args = append(bulkPayload.Args, addExistingTableOrViewPayload)
		default:
			return nil, fmt.Errorf("request type %s is not recocogonised", requestType)
		}

	}
	return bulkPayload, nil
}
