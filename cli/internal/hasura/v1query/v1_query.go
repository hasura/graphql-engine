package v1query

import (
	"bytes"
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/hasura/commonmetadata"
	"github.com/hasura/graphql-engine/cli/internal/hasura/databaseops"
	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

type Client struct {
	*httpc.Client
	// api path, normaly this would be v1/query
	path string

	*databaseops.ClientDatabaseOps
	*commonmetadata.ClientCommonMetadataOps
}

func New(c *httpc.Client, path string) *Client {
	client := &Client{
		Client:                  c,
		path:                    path,
		ClientDatabaseOps:       databaseops.New(c, path),
		ClientCommonMetadataOps: commonmetadata.New(c, path),
	}
	return client
}

func (c *Client) Send(body interface{}) (*httpc.Response, io.Reader, error) {
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
