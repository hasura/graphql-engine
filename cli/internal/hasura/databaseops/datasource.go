package databaseops

import (
	"bytes"
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

// allow to interact with all hasura opertions on database
type ClientDatabaseOps struct {
	*httpc.Client
	// api subpath eg: "v1/query"
	path string
}

func New(client *httpc.Client, path string) *ClientDatabaseOps {
	return &ClientDatabaseOps{client, path}
}

func (h *ClientDatabaseOps) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
	req, err := h.NewRequest(http.MethodPost, h.path, body)
	if err != nil {
		return nil, err
	}
	resp, err := h.LockAndDo(context.Background(), req, responseBodyWriter)
	if err != nil {
		return nil, err
	}
	return resp, nil
}

func (c *ClientDatabaseOps) SendDatabaseOperation(body interface{}) (*httpc.Response, io.Reader, error) {
	req, err := c.NewRequest(http.MethodPost, c.path, body)
	if err != nil {
		return nil, nil, err
	}
	responseBody := new(bytes.Buffer)
	resp, err := c.LockAndDo(context.Background(), req, responseBody)
	if err != nil {
		return resp, nil, err
	}
	return resp, responseBody, nil
}
