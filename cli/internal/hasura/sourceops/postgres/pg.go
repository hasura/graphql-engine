package postgres

import (
	"context"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
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

func (d *SourceOps) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
	var op errors.Op = "postgres.SourceOps.send"
	req, err := d.NewRequest(http.MethodPost, d.path, body)
	if err != nil {
		return nil, errors.E(op, err)
	}
	resp, err := d.LockAndDo(context.Background(), req, responseBodyWriter)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return resp, nil
}
