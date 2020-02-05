package types

import "encoding/json"

type Common struct {
	Actions     []Action    `json:"actions" yaml:"actions"`
	CustomTypes CustomTypes `json:"custom_types" yaml:"custom_types"`
}

func (c *Common) SetExportDefault() {
	for index := range c.Actions {
		c.Actions[index].Definition.Arguments = nil
		c.Actions[index].Definition.OutputType = ""
	}

	for index := range c.CustomTypes.Enums {
		c.CustomTypes.Enums[index].Fields = nil
	}

	for index := range c.CustomTypes.InputObjects {
		c.CustomTypes.InputObjects[index].Fields = nil
	}

	for index := range c.CustomTypes.Objects {
		c.CustomTypes.Objects[index].Fields = nil
	}

	for index := range c.CustomTypes.Scalars {
		c.CustomTypes.Scalars[index].Fields = nil
	}
}

type Action struct {
	Name        string        `json:"name" yaml:"name"`
	Definition  ActionDef     `json:"definition" yaml:"definition"`
	Permissions []interface{} `json:"permissions" yaml:"permissions"`
}

func (a *Action) MarshalJSON() ([]byte, error) {
	if a.Permissions == nil {
		a.Permissions = make([]interface{}, 0)
	}
	type t Action
	return json.Marshal(&struct {
		*t
	}{
		t: (*t)(a),
	})
}

type ActionDef struct {
	Arguments  []interface{} `json:"arguments" yaml:"arguments,omitempty"`
	OutputType string        `json:"output_type" yaml:"output_type,omitempty"`
	Kind       string        `json:"kind" yaml:"kind"`
	Handler    string        `json:"handler" yaml:"handler"`
}

type CustomTypes struct {
	Enums        []CustomTypeDef `json:"enums" yaml:"enums"`
	InputObjects []CustomTypeDef `json:"input_objects" yaml:"input_objects"`
	Objects      []CustomTypeDef `json:"objects" yaml:"objects"`
	Scalars      []CustomTypeDef `json:"scalars" yaml:"scalars"`
}

type CustomTypeDef struct {
	Name          string        `json:"name" yaml:"name"`
	Description   *string       `json:"description" yaml:"description,omitempty"`
	Fields        []interface{} `json:"fields,omitempty" yaml:"fields,omitempty"`
	Values        []interface{} `json:"values,omitempty" yaml:"values,omitempty"`
	Relationships []interface{} `json:"relationships,omitempty" yaml:"relationships"`
}

func (c *CustomTypeDef) MarshalJSON() ([]byte, error) {
	if c.Relationships == nil {
		c.Relationships = make([]interface{}, 0)
	}
	type t CustomTypeDef
	return json.Marshal(&struct {
		*t
	}{
		t: (*t)(c),
	})
}
