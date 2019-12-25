package types

type Action struct {
	Name        string                   `json:"name" yaml:"name"`
	Definition  ActionDef                `json:"definition" yaml:"definition"`
	Permissions []map[string]interface{} `json:"permissions" yaml:"permissions"`
}

type ActionDef struct {
	Arguments  []map[string]interface{} `json:"arguments" yaml:"arguments,omitempty"`
	OutputType string                   `json:"output_type" yaml:"output_type,omitempty"`
	Kind       string                   `json:"kind" yaml:"kind"`
	Webhook    string                   `json:"webhook" yaml:"webhook"`
}

func (a ActionDef) MarshalYAML() (interface{}, error) {
	a.Arguments = nil
	a.OutputType = ""
	return a, nil
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
	Fields        []interface{} `json:"fields" yaml:"fields,omitempty"`
	Relationships []interface{} `json:"relationships" yaml:"relationships,omitempty"`
}

func (c CustomTypeDef) MarshalYAML() (interface{}, error) {
	c.Fields = nil
	return c, nil
}
