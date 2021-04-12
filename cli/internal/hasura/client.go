package hasura

import (
	"io"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

type GenericSend func(requestBody interface{}) (httpcResponse *httpc.Response, responseBody io.Reader, error error)

type Client struct {
	V1Metadata V1Metadata
	V1Query    V1Query
	V2Query    V2Query
	PGDump     PGDump
	V1Graphql  V1Graphql
}

type V1Query interface {
	CommonMetadataOperations
	PGSourceOps
	Send(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
	Bulk([]RequestBody) (io.Reader, error)
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

type SourceKind string

const (
	SourceKindPG    SourceKind = "postgres"
	SourceKindMSSQL            = "mssql"
)

type V2Query interface {
	PGSourceOps
	MSSQLSourceOps
	Send(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
	Bulk([]RequestBody) (io.Reader, error)
}

type IntrospectionSchema interface{}
type V1Graphql interface {
	GetIntrospectionSchema() (IntrospectionSchema, error)
}
type RequestBody struct {
	Type    string      `json:"type"`
	Version uint        `json:"version,omitempty"`
	Args    interface{} `json:"args"`
}
