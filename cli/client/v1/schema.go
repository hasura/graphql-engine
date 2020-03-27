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
	SQL                      string `json:"sql"`
	Cascade                  bool   `json:"cascade"`
	CheckMetadataConsistency bool   `json:"check_metadata_consistency"`
	ReadOnly                 bool   `json:"read_only"`
}

func (p *RunSQLPayload) ResetArgs() {
	p.Args = RunSQLArgs{}
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

func (p *AddExistingTableOrViewPayload) ResetArgs() {
	p.Args = AddExistingTableOrViewArgs{}
}
func (p *AddExistingTableOrViewPayload) Validate() error {
	if p.Type != AddExistingTableOrView {
		return makePayloadValidationErr(RunSQL, fmt.Sprintf("bad request type provided: %v", p.Type))
	}
	return nil
}

const Bulk = "bulk"

type BulkPayload struct {
	Type string        `json:"type"`
	Args []SendPayload `json:"args"`
}

func (b *BulkPayload) ResetArgs() {
	b.Args = nil
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

type SendPayload interface {
	Validate() error
	ResetArgs()
}

func (s *QueryService) Send(payload SendPayload) (*http.Response, []byte, *Error) {
	if err := payload.Validate(); err != nil {
		return nil, nil, E(err)
	}
	request, err := s.client.NewRequest("POST", schemaMetadataAPIEndpoint, payload)
	if err != nil {
		return nil, nil, E(err)
	}

	resp, err := s.client.Do(request)
	if err != nil {
		return nil, nil, E(errors.Wrap(err, "error from sending request"))
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
func BulkPayloadMaker(payload []byte) (*BulkPayload, error) {
	var bulkPayload = new(BulkPayload)
	bulkPayload.Type = Bulk
	var payloads = make([]map[string]interface{}, 0)
	err := json.Unmarshal(payload, &payloads)
	if err != nil {
		return nil, errors.Wrapf(err, "cannot unmarshal payload %v", payload)
	}
	for _, payload := range payloads {
		payloadWithUnderlyingType, err := PayloadMaker(payload)
		if err != nil {
			return nil, err
		}
		bulkPayload.Args = append(bulkPayload.Args, payloadWithUnderlyingType)
	}

	return bulkPayload, nil
}

// PayloadMaker will make a payload with correct underlying type when a json byte array is given
func PayloadMaker(payload map[string]interface{}) (SendPayload, error) {
	requestType, ok := payload["type"]
	if !ok {
		return nil, fmt.Errorf("request type not found in payload %v", payload)
	}
	// According to the request type Unmarshal the json to correct request type
	switch requestType {
	case RunSQL:
		var runSQLPayload = new(RunSQLPayload)
		runSQLPayload.Type = RunSQL
		err := mapstructure.Decode(payload, runSQLPayload)
		if err != nil {
			return nil, errors.Wrapf(err, "error decoding payload %v", payload)
		}
		return runSQLPayload, nil
	case AddExistingTableOrView:
		var addExistingTableOrViewPayload = new(AddExistingTableOrViewPayload)
		addExistingTableOrViewPayload.Type = AddExistingTableOrView
		err := mapstructure.Decode(payload, addExistingTableOrViewPayload)
		if err != nil {
			return nil, errors.Wrapf(err, "error decoding payload %v", payload)
		}
		return addExistingTableOrViewPayload, nil
	default:
		return nil, fmt.Errorf("request type %s is not recocogonised", requestType)
	}

	return nil, fmt.Errorf("internal error while trying to construct a payload of type %v", requestType)
}
