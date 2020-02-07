package types

import "gopkg.in/yaml.v2"

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
	Name        string          `json:"name" yaml:"name"`
	Definition  ActionDef       `json:"definition" yaml:"definition"`
	Permissions []yaml.MapSlice `json:"-" yaml:"permissions,omitempty"`
}

type ActionDef struct {
	Kind                 string          `json:"kind" yaml:"kind"`
	Handler              string          `json:"handler" yaml:"handler"`
	Arguments            []yaml.MapSlice `json:"arguments" yaml:"arguments,omitempty"`
	OutputType           string          `json:"output_type" yaml:"output_type,omitempty"`
	ForwardClientHeaders bool            `json:"-" yaml:"forward_client_headers,omitempty"`
	Headers              []yaml.MapSlice `json:"-" yaml:"headers,omitempty"`
}

type CustomTypes struct {
	Enums        []CustomTypeDef `json:"enums" yaml:"enums"`
	InputObjects []CustomTypeDef `json:"input_objects" yaml:"input_objects"`
	Objects      []CustomTypeDef `json:"objects" yaml:"objects"`
	Scalars      []CustomTypeDef `json:"scalars" yaml:"scalars"`
}

type CustomTypeDef struct {
	Name          string          `json:"name" yaml:"name"`
	Description   *string         `json:"description" yaml:"description,omitempty"`
	Fields        []yaml.MapSlice `json:"fields,omitempty" yaml:"fields,omitempty"`
	Values        []interface{}   `json:"values,omitempty" yaml:"values,omitempty"`
	Relationships []yaml.MapSlice `json:"-" yaml:"relationships,omitempty"`
}
