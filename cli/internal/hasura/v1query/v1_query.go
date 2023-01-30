package v1query

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/commonmetadata"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type Client struct {
	*httpc.Client
	// api path, normaly this would be v1/query
	path string

	*postgres.SourceOps
	*commonmetadata.ClientCommonMetadataOps
}

func New(c *httpc.Client, path string) *Client {
	client := &Client{
		Client:                  c,
		path:                    path,
		SourceOps:               postgres.New(c, path),
		ClientCommonMetadataOps: commonmetadata.New(c, path),
	}
	return client
}

func (c *Client) Send(body interface{}) (*httpc.Response, io.Reader, error) {
	var op errors.Op = "v1query.Client.Send"
	req, err := c.NewRequest(http.MethodPost, c.path, body)
	if err != nil {
		return nil, nil, errors.E(op, err)
	}
	responseBody := new(bytes.Buffer)
	resp, err := c.LockAndDo(context.Background(), req, responseBody)
	if err != nil {
		return resp, nil, errors.E(op, err)
	}
	return resp, responseBody, nil
}

func (c *Client) Bulk(args []hasura.RequestBody) (io.Reader, error) {
	var op errors.Op = "v1query.Client.Bulk"
	body := hasura.RequestBody{
		Type: "bulk",
		Args: args,
	}
	req, err := c.NewRequest(http.MethodPost, c.path, body)
	if err != nil {
		return nil, errors.E(op, err)
	}
	responseBody := new(bytes.Buffer)
	resp, err := c.LockAndDo(context.Background(), req, responseBody)
	if err != nil {
		return nil, errors.E(op, err)
	} else if resp.StatusCode != http.StatusOK {
		return nil, errors.E(op, errors.KindHasuraAPI, fmt.Errorf("bulk request failed: %v %v", resp.StatusCode, responseBody.String()))
	}
	return responseBody, nil
}
