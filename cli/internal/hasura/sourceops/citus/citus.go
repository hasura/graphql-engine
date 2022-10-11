package citus

import (
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type SourceOps struct {
	*httpc.Client
	// api subpath eg: "v1/query"
	path string
}

func New(client *httpc.Client, path string) *SourceOps {
	return &SourceOps{client, path}
}

func (d *SourceOps) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
	req, err := d.NewRequest(http.MethodPost, d.path, body)
	if err != nil {
		return nil, err
	}
	resp, err := d.LockAndDo(context.Background(), req, responseBodyWriter)
	if err != nil {
		return nil, err
	}
	return resp, nil
}
