package hasura

import (
	"io"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

const (
	CommandOK RunSQLResultType = "CommandOk"
	TuplesOK  RunSQLResultType = "TuplesOk"
)

// hasura API requests used to interact with connected database(s)
type DatabaseOperations interface {
	RunSQL(input RunSQLInput) (response *RunSQLOutput, err error)
	SendDatabaseOperation(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
}

type RunSQLInput struct {
	SQL                      string `json:"sql" yaml:"sql"`
	Source                   string `json:"source,omitempty" yaml:"source,omitempty"`
	Cascade                  bool   `json:"cascade,omitempty" yaml:"cascade,omitempty"`
	ReadOnly                 bool   `json:"read_only,omitempty" yaml:"read_only,omitempty"`
	CheckMetadataConsistency *bool  `json:"check_metadata_consistency,omitempty" yaml:"check_metadata_consistency,omitempty"`
}

type RunSQLResultType string

type RunSQLOutput struct {
	ResultType RunSQLResultType `json:"result_type" yaml:"result_type"`
	Result     [][]string       `json:"result" yaml:"result"`
}

type RequestTypes string

type MetadataVersion int
