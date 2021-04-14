package catalogstate

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

type ClientCatalogState struct {
	*httpc.Client
	path string
}

func New(client *httpc.Client, path string) *ClientCatalogState {
	return &ClientCatalogState{client, path}
}

func (c *ClientCatalogState) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
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

func (c ClientCatalogState) Set(key string, state interface{}) (io.Reader, error) {
	request := hasura.RequestBody{
		Type: "set_catalog_state",
		Args: map[string]interface{}{
			"type":  key,
			"state": state,
		},
	}
	responseBody := new(bytes.Buffer)
	response, err := c.send(request, responseBody)
	if err != nil {
		return nil, err
	}
	if response.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("%s", responseBody.String())
	}
	return responseBody, nil
}

func (c ClientCatalogState) Get() (io.Reader, error) {
	request := hasura.RequestBody{
		Type: "get_catalog_state",
		Args: map[string]string{},
	}
	responseBody := new(bytes.Buffer)
	response, err := c.send(request, responseBody)
	if err != nil {
		return nil, err
	}
	if response.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("%s", responseBody.String())
	}
	return responseBody, nil
}
