package hasuradb

import (
	"encoding/json"
	"fmt"
	"strings"
)

// Response is the response from GraphQL server
type Response struct {
	Data   *json.RawMessage `json:"data"`
	Errors *json.RawMessage `json:"errors"`
}

// Error is a the GraphQL error from server
type Error struct {
	Message    string           `json:"message"`
	Locations  []ErrorLocation  `json:"locations"`
	Type       string           `json:"type"`
	Path       []interface{}    `json:"path"`
	Extensions HasuraExtensions `json:"extensions"`
}

// Error returns the error message
func (e Error) Error() string {
	return e.Message
}

// HasuraExtensions is the error extension by Hasura
type HasuraExtensions struct {
	Path string `json:"path"`
	Code string `json:"code"`
}

// Errors are an array of GraphQL errors
type Errors []Error

// Error returns all error messages from Errors object
func (e Errors) Error() string {
	errors := []string{}
	for _, err := range e {
		errors = append(errors, err.Message)
	}
	return strings.Join(errors, ", ")
}

// ErrorLocation is the location of error in the query string
type ErrorLocation struct {
	Line   int `json:"line"`
	Column int `json:"column"`
}

func (h *HasuraDB) GetIntroSpectionSchema() (interface{}, error) {
	query := map[string]string{
		"query": "\n    query IntrospectionQuery {\n      __schema {\n        queryType { name }\n        mutationType { name }\n        subscriptionType { name }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          description\n          locations\n          args {\n            ...InputValue\n          }\n        }\n      }\n    }\n\n    fragment FullType on __Type {\n      kind\n      name\n      description\n      fields(includeDeprecated: true) {\n        name\n        description\n        args {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        description\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n\n    fragment InputValue on __InputValue {\n      name\n      description\n      type { ...TypeRef }\n      defaultValue\n    }\n\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }\n  ",
	}
	resp, _, err := h.sendv1GraphQL(query)
	if err != nil {
		return nil, err
	}
	var response Response
	err = json.NewDecoder(resp.Body).Decode(&response)
	if err != nil {
		return nil, err
	}
	var data map[string]interface{}
	if response.Data != nil {
		err = json.Unmarshal(*response.Data, &data)
		if err != nil {
			return nil, err
		}
	}

	if response.Errors != nil {
		var errors Errors
		err = json.Unmarshal(*response.Errors, &errors)
		if err != nil {
			return nil, err
		}
		return nil, fmt.Errorf("%v", errors)
	}
	return data, nil
}
