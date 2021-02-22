package hasura

import (
	"io"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

type Client struct {
	V1Metadata V1Metadata
	V1Query    V1Query
	V2Query    V2Query
}

type V1Query interface {
	CommonMetadataOperations
	DatabaseOperations
	Send(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
}

type V1Metadata interface {
	CommonMetadataOperations
	V2CommonMetadataOperations
	CatalogStateOperations
	Send(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
}

type CatalogStateOperations interface {
	Set(key string, state interface{}) (io.Reader, error)
	Get() (io.Reader, error)
}

type V2Query interface {
	DatabaseOperations
	Send(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
}

type RequestBody struct {
	Type    string      `json:"type"`
	Version uint        `json:"version,omitempty"`
	Args    interface{} `json:"args"`
}
