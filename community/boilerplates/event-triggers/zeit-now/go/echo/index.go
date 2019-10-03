package main

import (
	"encoding/json"
	"fmt"
	"net/http"
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
	Old map[string]interface{} `json:"old"`
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

type TriggerResponse struct {
	Message string                 `json:"message"`
	OldData map[string]interface{} `json:"oldData"`
	NewData map[string]interface{} `json:"newData"`
}

func Handler(w http.ResponseWriter, r *http.Request) {
	decoder := json.NewDecoder(r.Body)
	var event HasuraEvent
	err := decoder.Decode(&event)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	response := TriggerResponse{
		Message: fmt.Sprintf(
			"got '%s' for '%s' operation on '%s' table in '%s' schema from '%s' trigger",
			event.ID,
			event.Event.Op,
			event.Table.Name,
			event.Table.Schema,
			event.Trigger.Name,
		),
		OldData: event.Data.Old,
		NewData: event.Data.New,
	}
	err = json.NewEncoder(w).Encode(response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}
