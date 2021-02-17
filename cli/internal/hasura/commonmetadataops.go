package hasura

import (
	"io"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

// general hasura metadata API requests
// these are not dependent on the connected source type
type CommonMetadataOperations interface {
	ExportMetadata() (metadata io.Reader, err error)
	ClearMetadata() (io.Reader, error)
	ReloadMetadata() (io.Reader, error)
	DropInconsistentMetadata() (io.Reader, error)
	ReplaceMetadata(metadata io.Reader) (io.Reader, error)
	GetInconsistentMetadata() (*GetInconsistentMetadataResponse, error)
	GetInconsistentMetadataReader() (io.Reader, error)
	SendCommonMetadataOperation(requestBody interface{}) (httpcResponse *httpc.Response, body io.Reader, error error)
}

type GetInconsistentMetadataResponse struct {
	IsConsistent        bool          `json:"is_consistent"`
	InconsistentObjects []interface{} `json:"inconsistent_objects"`
}
