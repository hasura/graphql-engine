package util

import (
	"encoding/json"

	"github.com/sirupsen/logrus"

	"github.com/Masterminds/semver"
	"github.com/parnurzeal/gorequest"
)

// ServerState is the state of Hasura stored on the server.
type ServerState struct {
	UUID string
	MiscState
}

// MiscState is the miscellaneous state values stored on the server by
// console and CLI.
type MiscState struct {
	CLIState `json:"cli"`
}

// CLIState is the state stored by CLI on the server.
type CLIState struct {
}

type runSQLResponse struct {
	ResultType string     `json:"result_type"`
	Result     [][]string `json:"result"`
}

// GetServerState queries a server for the state.
func GetServerState(endpoint, accessKey string, serverVersion *semver.Version, log *logrus.Logger) *ServerState {
	state := &ServerState{
		UUID: "00000000-0000-0000-0000-000000000000",
	}
	payload := `{
		"type": "run_sql",
		"args": {
			"sql": "select hasura_uuid, misc_state from hdb_catalog.hdb_version"
		}
	}`
	req := gorequest.New()
	req = req.Post(endpoint + "/v1/query").Send(payload)
	req.Set("X-Hasura-Access-Key", accessKey)

	var r runSQLResponse
	_, _, errs := req.EndStruct(&r)
	if len(errs) != 0 {
		log.Debugf("server state: errors: %v", errs)
		return state
	}

	if r.ResultType != "TuplesOk" {
		log.Debugf("server state: resultType: %s", r.ResultType)
		return state
	}

	if len(r.Result) != 2 {
		log.Debugf("server state: result: %v", r.Result)
		return state
	}

	header := r.Result[0]
	var i_uuid, i_state int
	for i, k := range header {
		if k == "hasura_uuid" {
			i_uuid = i
		}
		if k == "misc_state" {
			i_state = i
		}
	}
	values := r.Result[1]

	state.UUID = values[i_uuid]

	var ms MiscState
	err := json.Unmarshal([]byte(values[i_state]), &ms)
	if err != nil {
		log.Debugf("server state: %v", values[i_state])
		return state
	}
	state.MiscState = ms
	return state
}
