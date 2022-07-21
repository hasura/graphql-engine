package event

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"github.com/Khan/genqlient/graphql"

	"github.com/hasura/graphql-engine/community/sample-apps/golang-backend-server/generated"
)

type EventTriggerPayload[Old interface{}, New interface{}] struct {
	Event struct {
		SessionVariables struct {
			XHasuraRole string `json:"x-hasura-role"`
		} `json:"session_variables"`
		Op   string `json:"op"`
		Data struct {
			Old *Old `json:"old"`
			New *New `json:"new"`
		} `json:"data"`
		TraceContext struct {
			TraceID string `json:"trace_id"`
			SpanID  string `json:"span_id"`
		} `json:"trace_context"`
	} `json:"event"`
	CreatedAt    time.Time `json:"created_at"`
	ID           string    `json:"id"`
	DeliveryInfo struct {
		MaxRetries   int `json:"max_retries"`
		CurrentRetry int `json:"current_retry"`
	} `json:"delivery_info"`
	Trigger struct {
		Name string `json:"name"`
	} `json:"trigger"`
	Table struct {
		Schema string `json:"schema"`
		Name   string `json:"name"`
	} `json:"table"`
}

func NewUserHandler(w http.ResponseWriter, r *http.Request) {
	var u EventTriggerPayload[interface{}, struct {
		Id   string
		Name string
	}]
	err := json.NewDecoder(r.Body).Decode(&u)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	fmt.Println("Hello", u.Event.Data.New.Name)

	ctx := context.Background()
	client := graphql.NewClient("http://graphql-engine:8080/v1/graphql", http.DefaultClient)
	resp, _ := generated.GetUsers(ctx, client)
	for _, value := range resp.GetUser() {
		fmt.Printf("%#v", value)
	}

	w.WriteHeader(200)
}
