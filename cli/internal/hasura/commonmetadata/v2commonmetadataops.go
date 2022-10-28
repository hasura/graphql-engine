package commonmetadata

import (
	"bytes"
	"encoding/json"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (c *ClientCommonMetadataOps) V2ReplaceMetadata(args hasura.V2ReplaceMetadataArgs) (*hasura.V2ReplaceMetadataResponse, error) {
	var op errors.Op = "commonmetadata.ClientCommonMetadataOps.V2ReplaceMetadata"
	request := hasura.RequestBody{
		Type:    "replace_metadata",
		Version: 2,
		Args:    args,
	}
	responseBody := new(bytes.Buffer)
	response, err := c.send(request, responseBody)
	if err != nil {
		return nil, errors.E(op, err)
	}
	if response.StatusCode != http.StatusOK {
		return nil, errors.E(op, errors.KindHasuraAPI, responseBody.String())
	}
	v2replaceMetadataResponse := new(hasura.V2ReplaceMetadataResponse)
	if err := json.NewDecoder(responseBody).Decode(v2replaceMetadataResponse); err != nil {
		return nil, errors.E(op, err)
	}
	return v2replaceMetadataResponse, nil
}
