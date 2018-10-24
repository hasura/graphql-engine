package main

import (
	"bytes"
	"encoding/json"
	"log"
	"net/http"
	"os"

	"github.com/aws/aws-lambda-go/events"
	"github.com/aws/aws-lambda-go/lambda"
)

type HasuraEvent struct {
	ID      string `json:"id"`
	Event   `json:"event"`
	Table   `json:"table"`
	Trigger `json:"trigger"`
}

type Event struct {
	Op   string `json:"op"`
	Data `json:"data"`
}

type Data struct {
	Old map[string]interface{} `json:"old  "`
	New map[string]interface{} `json:"new"`
}

type Table struct {
	Name   string `json:"name"`
	Schema string `json:"schema"`
}

type Trigger struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

const MUTATION_UPDATE_NOTE_REVISION = `
  mutation updateNoteRevision ($object: note_revision_insert_input!) {
    insert_note_revision (objects: [$object]) {
      affected_rows
      returning {
        id
      }
    }
  }
`

func constructErrorResponse(responsePayload map[string]string) (events.APIGatewayProxyResponse, error) {
	var responseBody []byte

	responseBody, err := json.Marshal(responsePayload)
	var statusCode int = 200

	if err != nil {
		responseBody, _ = json.Marshal(map[string]string{
			"message": "Internal error ocurred while constructing error response",
		})
		statusCode = 500
	}

	return events.APIGatewayProxyResponse{
		Body:       string(responseBody),
		StatusCode: statusCode,
	}, nil
}

var HGE_ENDPOINT = os.Getenv("HGE_ENDPOINT")

// Handler for the Lambda function to echo back the data
func Handler(request events.APIGatewayProxyRequest) (events.APIGatewayProxyResponse, error) {
	// stdout and stderr are sent to AWS CloudWatch Logs
	log.Printf("Processing Lambda request %+v\n", request.Body)

	if len(HGE_ENDPOINT) == 0 {
		return constructErrorResponse(map[string]string{
			"message": "HGE Endpoint not defined in environment variable",
		})
	}

	// parse json body
	var body HasuraEvent

	err := json.Unmarshal([]byte(request.Body), body)

	event := body.Event

	if err != nil {
		return constructErrorResponse(map[string]string{
			"message": "Unable to parse Hasura Event",
		})
	}

	note_id, ok := event.Data.Old["id"]
	if !ok {
		return constructErrorResponse(map[string]string{
			"message": "invalid payload: note id not found",
		})
	}
	note, ok := event.Data.New["note"]
	if !ok {
		return constructErrorResponse(map[string]string{
			"message": "invalid payload: note not found",
		})
	}

	// execute the mutation
	payload := map[string]interface{}{
		"query": MUTATION_UPDATE_NOTE_REVISION,
		"variables": map[string]interface{}{
			"object": map[string]interface{}{
				"note_id": note_id.(float64),
				"note":    note.(string),
			},
		},
	}

	b := new(bytes.Buffer)
	json.NewEncoder(b).Encode(payload)
	res, err := http.Post(HGE_ENDPOINT, "application/json; charset=utf-8", b)
	if err != nil {
		return constructErrorResponse(map[string]string{
			"message": err.Error(),
		})
	}
	defer res.Body.Close()
	var response map[string]interface{}

	err = json.NewDecoder(res.Body).Decode(&response)
	if err != nil {
		return constructErrorResponse(map[string]string{
			"message": err.Error(),
		})
	}

	responseBody, err := json.Marshal(response)

	if err != nil {
		return constructErrorResponse(map[string]string{
			"message": err.Error(),
		})
	}

	return events.APIGatewayProxyResponse{
		Body:       string(responseBody),
		StatusCode: 200,
	}, nil
}

func main() {
	lambda.Start(Handler)
}
