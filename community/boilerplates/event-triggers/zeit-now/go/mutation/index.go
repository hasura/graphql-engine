package main

import (
	"bytes"
	"encoding/json"
	"net/http"
	"os"
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

var HGE_ENDPOINT = os.Getenv("HGE_ENDPOINT")
func Handler(w http.ResponseWriter, r *http.Request) {
	decoder := json.NewDecoder(r.Body)
	var event HasuraEvent
	err := decoder.Decode(&event)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	note_id, ok := event.Data.Old["id"]
	if !ok {
		http.Error(w, "invalid payload: note id not found", http.StatusBadRequest)
		return
	}
	note, ok := event.Data.New["note"]
	if !ok {
		http.Error(w, "invalid payload: note not found", http.StatusBadRequest)
		return
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
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer res.Body.Close()
	var response map[string]interface{}

	err = json.NewDecoder(res.Body).Decode(&response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}

	err = json.NewEncoder(w).Encode(response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}
