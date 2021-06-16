package commonmetadata

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (c *ClientCommonMetadataOps) V2ReplaceMetadata(args hasura.V2ReplaceMetadataArgs) (*hasura.V2ReplaceMetadataResponse, error) {
	request := hasura.RequestBody{
		Type:    "replace_metadata",
		Version: 2,
		Args:    args,
	}
	responseBody := new(bytes.Buffer)
	response, err := c.send(request, responseBody)
	if err != nil {
		return nil, err
	}
	if response.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("%s", responseBody.String())
	}
	v2replaceMetadataResponse := new(hasura.V2ReplaceMetadataResponse)
	if err := json.NewDecoder(responseBody).Decode(v2replaceMetadataResponse); err != nil {
		return nil, err
	}
	return v2replaceMetadataResponse, nil
}
