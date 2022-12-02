package v1metadata

import (
	"bytes"
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/catalogstate"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/commonmetadata"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type Client struct {
	client *httpc.Client
	path   string

	*commonmetadata.ClientCommonMetadataOps
	*catalogstate.ClientCatalogState
}

func (c *Client) Send(body interface{}) (*httpc.Response, io.Reader, error) {
	var op errors.Op = "v1metadata.Client.Send"
	req, err := c.client.NewRequest(http.MethodPost, c.path, body)
	if err != nil {
		return nil, nil, errors.E(op, err)
	}
	var responseBody = new(bytes.Buffer)
	resp, err := c.client.LockAndDo(context.Background(), req, responseBody)
	if err != nil {
		return resp, nil, errors.E(op, err)
	}
	return resp, responseBody, nil
}

func New(c *httpc.Client, path string) *Client {
	client := &Client{
		client:                  c,
		path:                    path,
		ClientCommonMetadataOps: commonmetadata.New(c, path),
		ClientCatalogState:      catalogstate.New(c, path),
	}
	return client
}
