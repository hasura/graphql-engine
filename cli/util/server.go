package util

import (
	"crypto/tls"

	"github.com/sirupsen/logrus"

	"github.com/Masterminds/semver"
	"github.com/parnurzeal/gorequest"
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
func GetServerState(endpoint, adminSecret string, config *tls.Config, serverVersion *semver.Version, log *logrus.Logger) *ServerState {
	state := &ServerState{
		UUID: "00000000-0000-0000-0000-000000000000",
	}
	payload := `{
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
	}`
	req := gorequest.New()
	if config != nil {
		req = req.TLSClientConfig(config)
	}
	req = req.Post(endpoint + "/v1/query").Send(payload)
	req.Set("X-Hasura-Admin-Secret", adminSecret)

	var r []hdbVersion
	_, _, errs := req.EndStruct(&r)
	if len(errs) != 0 {
		log.Debugf("server state: errors: %v", errs)
		return state
	}

	if len(r) != 1 {
		log.Debugf("invalid response: %v", r)
		return state
	}

	state.UUID = r[0].UUID
	state.CLIState = r[0].CLIState
	return state
}
