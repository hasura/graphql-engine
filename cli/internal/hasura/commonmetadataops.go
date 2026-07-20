package hasura

import (
	"io"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

// CommonMetadataOperations represents Metadata API's which are not source type specific.
type CommonMetadataOperations interface {
	ExportMetadata() (metadata io.Reader, err error)
	ClearMetadata() (io.Reader, error)
	ReloadMetadata() (io.Reader, error)
	DropInconsistentMetadata() (io.Reader, error)
	ReplaceMetadata(metadata io.Reader) (io.Reader, error)
	GetInconsistentMetadata() (*GetInconsistentMetadataResponse, error)
	GetInconsistentMetadataRaw() (io.Reader, error)
	SendCommonMetadataOperation(requestBody any) (*httpc.Response, io.Reader, error)
}

type V2CommonMetadataOperations interface {
	V2ReplaceMetadata(args V2ReplaceMetadataArgs) (*V2ReplaceMetadataResponse, error)
}

type V2ReplaceMetadataArgs struct {
	AllowInconsistentMetadata bool `json:"allow_inconsistent_metadata"`
	Metadata                  any  `json:"metadata"`
}

type V2ReplaceMetadataResponse struct {
	IsConsistent        bool `json:"is_consistent"`
	InconsistentObjects any  `json:"inconsistent_objects"`
}

type GetInconsistentMetadataResponse struct {
	IsConsistent        bool  `json:"is_consistent"`
	InconsistentObjects []any `json:"inconsistent_objects"`
}
