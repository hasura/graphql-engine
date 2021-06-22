package pgdump

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

type Client struct {
	*httpc.Client
	path string
}

func New(client *httpc.Client, path string) *Client {
	return &Client{client, path}
}

func (c *Client) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
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

func (c *Client) Send(request hasura.PGDumpRequest) (io.Reader, error) {
	responseBody := new(bytes.Buffer)
	response, err := c.send(request, responseBody)
	if err != nil {
		return nil, err
	}
	if response.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("pg_dump request: %d \n%s", response.StatusCode, responseBody.String())
	}
	return responseBody, nil
}
