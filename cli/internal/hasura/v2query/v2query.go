package v2query

import (
	"bytes"
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/bigquery"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/citus"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/cockroach"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/mssql"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura/sourceops/postgres"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type Client struct {
	*httpc.Client
	hasura.PGSourceOps
	hasura.MSSQLSourceOps
	hasura.CitusSourceOps
	hasura.BigQuerySourceOps
	hasura.CockroachSourceOps
	path string
}

func New(c *httpc.Client, path string) *Client {
	client := &Client{
		Client:             c,
		PGSourceOps:        postgres.New(c, path),
		MSSQLSourceOps:     mssql.New(c, path),
		CitusSourceOps:     citus.New(c, path),
		CockroachSourceOps: cockroach.New(c, path),
		BigQuerySourceOps:  bigquery.New(c, path),
		path:               path,
	}
	return client
}

func (c *Client) Send(body interface{}) (*httpc.Response, io.Reader, error) {
	var op errors.Op = "v2query.Client.Send"
	req, err := c.NewRequest(http.MethodPost, c.path, body)
	if err != nil {
		return nil, nil, errors.E(op, err)
	}
	var responseBody = new(bytes.Buffer)
	resp, err := c.LockAndDo(context.Background(), req, responseBody)
	if err != nil {
		return resp, nil, errors.E(op, err)
	}
	return resp, responseBody, nil
}

func (c *Client) Bulk(args []hasura.RequestBody) (io.Reader, error) {
	var op errors.Op = "v2query.Client.Bulk"
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
		return nil, errors.E(op, errors.KindHasuraAPI, responseBody.String())
	}
	return responseBody, nil
}
