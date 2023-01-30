package v1version

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

type Client struct {
	*httpc.Client
	path string
}

func New(client *httpc.Client, path string) *Client {
	return &Client{client, path}
}

func (c *Client) GetVersion() (*hasura.V1VersionResponse, error) {
	var op errors.Op = "v1version.Client.GetVersion"
	b := new(bytes.Buffer)
	resp, err := c.send(nil, b)
	if err != nil {
		return nil, errors.E(op, err)
	}

	if resp.StatusCode != http.StatusOK {
		if b.Len() > 0 {
			return nil, errors.E(op, errors.KindHasuraAPI, b.String())
		} else {
			return nil, errors.E(op, errors.KindHasuraAPI, fmt.Errorf("API request to %v failed, code: %v", c.path, resp.StatusCode))
		}
	}
	o := new(hasura.V1VersionResponse)
	if err := json.NewDecoder(b).Decode(o); err != nil {
		return nil, errors.E(op, fmt.Errorf("decoding API response failed for: %v", c.path))
	}
	return o, nil
}

func (c *Client) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
	var op errors.Op = "v1version.Client.send"
	req, err := c.NewRequest(http.MethodGet, c.path, body)
	if err != nil {
		return nil, errors.E(op, err)
	}
	resp, err := c.LockAndDo(context.Background(), req, responseBodyWriter)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return resp, nil
}
