package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/aws/aws-lambda-go/events"
	"github.com/aws/aws-lambda-go/lambda"
)

type TableStruct struct {
	Name string `json:"name"`
}

type EventData struct {
	Old map[string]interface{} `json:"old"`
	New map[string]interface{} `json"new"`
}

type EventStruct struct {
	Operation string    `json:"op"`
	Data      EventData `json:"data"`
}

type HasuraEvent struct {
	Table *TableStruct `json:"table"`
	Event *EventStruct `json:"event"`
	Op    string       `json:"op"`
}

// Handler for the Lambda function to echo back the data
func Handler(request events.APIGatewayProxyRequest) (events.APIGatewayProxyResponse, error) {

	// stdout and stderr are sent to AWS CloudWatch Logs
	log.Printf("Processing Lambda request %+v\n", request.Body)

	// parse json body
	body := &HasuraEvent{
		Table: &TableStruct{},
		Event: &EventStruct{},
	}

	err := json.Unmarshal([]byte(request.Body), body)

	if err != nil {
		message := map[string]string{
			"message": "Unable to parse Hasura Event",
		}

		responseBody, _ := json.Marshal(message)

		return events.APIGatewayProxyResponse{
			Body:       string(responseBody),
			StatusCode: 400,
		}, nil
	}

	var message = "cannot process request"
	var data = body.Event.Data

	if body.Table.Name == "notes" {
		switch body.Event.Operation {
		case "INSERT":
			message = fmt.Sprintf("New note %v inserted, with data: %v", data.New["id"], data.New["note"])
		case "UPDATE":
			message = fmt.Sprintf("New note %v updated, with data: %v", data.New["id"], data.New["note"])
		case "DELETE":
			message = fmt.Sprintf("New note %v delete, with data: %v", data.Old["id"], data.Old["note"])
		}
	}

	resposeBody, _ := json.Marshal(message)

	return events.APIGatewayProxyResponse{
		Body:       string(resposeBody),
		StatusCode: 200,
	}, nil
}

func main() {
	lambda.Start(Handler)
}
