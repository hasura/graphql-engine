package util

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"

	"github.com/sirupsen/logrus"
)

// ServerState is the state of Hasura stored on the server.
type ServerState struct {
	UUID     string
	CLIState map[string]interface{}
}

type hdbVersion struct {
	UUID     string                 `json:"hasura_uuid"`
	CLIState map[string]interface{} `json:"cli_state"`
}

// GetServerState queries a server for the state.
func GetServerState(client *httpc.Client, endpoint string, hasMetadataV3 bool, log *logrus.Logger) *ServerState {
	state := &ServerState{
		UUID: "00000000-0000-0000-0000-000000000000",
	}

	if hasMetadataV3 {
		payload := []byte(`
	{
    "type": "get_catalog_state",
    "args": {}
	}
`)
		var r struct {
			ID string `json:"id"`
		}
		var body interface{}
		err := json.Unmarshal(payload, &body)
		if err != nil {
			log.Debugf("unmarshalling json request to construct server state failed: %v", err)
			return state
		}
		req, err := client.NewRequest(http.MethodPost, endpoint, body)
		if err != nil {
			log.Debugf("constructing http request to construct server state failed: %v", err)
			return state
		}

		_, err = client.Do(context.Background(), req, &r)
		if err != nil {
			log.Debugf("http request to construct server state failed: %v", err)
			return state
		}

		state.UUID = r.ID
	} else {
		payload := []byte(`{
		"type": "select",
		"args": {
			"table": {
				"schema": "hdb_catalog",
				"name": "hdb_version"
			},
			"columns": [
				"hasura_uuid",
				"cli_state"
			]
		}
	}`)
		var body interface{}
		err := json.Unmarshal(payload, &body)
		if err != nil {
			log.Debugf("unmarshalling json request to construct server state failed: %v", err)
			return state
		}
		req, err := client.NewRequest(http.MethodPost, endpoint, body)
		if err != nil {
			log.Debugf("constructing http request to construct server state failed: %v", err)
			return state
		}
		var r []hdbVersion
		_, err = client.Do(context.Background(), req, &r)
		if err != nil {
			log.Debugf("http request to construct server state failed: %v", err)
			return state
		}
		if len(r) >= 1 {
			state.UUID = r[0].UUID
			state.CLIState = r[0].CLIState
		}
	}
	return state

}

func GetServerStatus(versionEndpoint string) (err error) {
	resp, err := http.Get(versionEndpoint)
	if err != nil {
		return fmt.Errorf("making http request failed: %w", err)
	}
	defer resp.Body.Close()
	// ignore error
	body, _ := ioutil.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("request failed: url: %s status code: %v status: %s \n%s", versionEndpoint, resp.StatusCode, resp.Status, string(body))
	}
	return nil
}
