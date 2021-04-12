package v1graphql

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/hasura/graphql-engine/cli/internal/httpc"
)

type Client struct {
	*httpc.Client
	path string
}

func New(client *httpc.Client, path string) *Client {
	return &Client{client, path}
}

func (c *Client) GetIntrospectionSchema() (hasura.IntrospectionSchema, error) {
	opName := "getIntrospectionSchema "
	query := map[string]string{
		"query": "\n    query IntrospectionQuery {\n      __schema {\n        queryType { name }\n        mutationType { name }\n        subscriptionType { name }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          description\n          locations\n          args {\n            ...InputValue\n          }\n        }\n      }\n    }\n\n    fragment FullType on __Type {\n      kind\n      name\n      description\n      fields(includeDeprecated: true) {\n        name\n        description\n        args {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        description\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n\n    fragment InputValue on __InputValue {\n      name\n      description\n      type { ...TypeRef }\n      defaultValue\n    }\n\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }\n  ",
	}
	responseBody := new(bytes.Buffer)
	var respBody struct {
		Data   *json.RawMessage `json:"data"`
		Errors *json.RawMessage `json:"errors"`
	}
	response, err := c.send(query, responseBody)
	if response.StatusCode != http.StatusOK {
		return nil, fmt.Errorf(" %s: %d \n%s", opName, response.StatusCode, responseBody.String())
	}
	if err != nil {
		return nil, err
	}
	err = json.NewDecoder(responseBody).Decode(&respBody)
	if err != nil {
		return nil, err
	}
	if respBody.Errors != nil {
		var b []byte
		if err := respBody.Errors.UnmarshalJSON(b); err != nil {
			return nil, fmt.Errorf("%s: decoding graphql response errors: %w", opName, err)
		}
		return nil, fmt.Errorf("%s: %w", opName, err)
	}
	var schema hasura.IntrospectionSchema
	if respBody.Data != nil {
		err = json.Unmarshal(*respBody.Data, &schema)
		if err != nil {
			return nil, fmt.Errorf("%s: decoding graphql responnse data: %w", opName, err)
		}
	}
	return schema, nil
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
