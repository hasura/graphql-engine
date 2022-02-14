package commonmetadata

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2/internal/httpc"
)

// implements all metadata operations which does not depend on the database
type ClientCommonMetadataOps struct {
	*httpc.Client
	path string
}

func New(client *httpc.Client, path string) *ClientCommonMetadataOps {
	return &ClientCommonMetadataOps{client, path}
}

func (c *ClientCommonMetadataOps) send(body interface{}, responseBodyWriter io.Writer) (*httpc.Response, error) {
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

func (c *ClientCommonMetadataOps) ExportMetadata() (metadata io.Reader, err error) {
	request := hasura.RequestBody{
		Type: "export_metadata",
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

func (c *ClientCommonMetadataOps) ClearMetadata() (io.Reader, error) {
	request := hasura.RequestBody{
		Type: "clear_metadata",
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

func (c *ClientCommonMetadataOps) ReloadMetadata() (metadata io.Reader, err error) {
	request := hasura.RequestBody{
		Type: "reload_metadata",
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

func (c *ClientCommonMetadataOps) DropInconsistentMetadata() (metadata io.Reader, err error) {
	request := hasura.RequestBody{
		Type: "drop_inconsistent_metadata",
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

func (c *ClientCommonMetadataOps) ReplaceMetadata(metadata io.Reader) (io.Reader, error) {
	var body interface{}
	if err := json.NewDecoder(metadata).Decode(&body); err != nil {
		return nil, fmt.Errorf("decoding json: %w", err)
	}
	request := hasura.RequestBody{
		Type: "replace_metadata",
		Args: body,
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

func (c *ClientCommonMetadataOps) GetInconsistentMetadata() (*hasura.GetInconsistentMetadataResponse, error) {
	inconsistentMetadata := new(hasura.GetInconsistentMetadataResponse)
	responseBody, err := c.GetInconsistentMetadataRaw()
	if err != nil {
		return nil, err
	}
	if err := json.NewDecoder(responseBody).Decode(inconsistentMetadata); err != nil {
		return nil, fmt.Errorf("decoding response: %w", err)
	}
	return inconsistentMetadata, nil
}

// GetInconsistentMetadataRaw
// https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/manage-metadata.html#metadata-get-inconsistent-metadata
func (c *ClientCommonMetadataOps) GetInconsistentMetadataRaw() (io.Reader, error) {
	request := hasura.RequestBody{
		Type: "get_inconsistent_metadata",
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

// SendCommonMetadataOperation send any request to metadata endpoint on hasura server by default this should be v1/metadata
func (c *ClientCommonMetadataOps) SendCommonMetadataOperation(body interface{}) (*httpc.Response, io.Reader, error) {
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
