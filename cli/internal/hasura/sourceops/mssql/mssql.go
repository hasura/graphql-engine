package mssql

import (
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

// allow to interact with all hasura opertions on database
type SourceOps struct {
	*httpc.Client
	// api subpath eg: "v1/query"
	path string
}

func New(client *httpc.Client, path string) *SourceOps {
	return &SourceOps{client, path}
}

func (c *SourceOps) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
	req, err := c.NewRequest(http.MethodPost, c.path, body)
	if err != nil {
		return nil, err
	}
	resp, err := c.LockAndDo(context.Background(), req, responseBodyWriter)
	if err != nil {
		return nil, err
	}
	return resp, nil
}
