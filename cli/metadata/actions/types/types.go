package types

import "gopkg.in/yaml.v2"

// CodegenExecutionConfig represents the config required to generate codegen for an action.
type CodegenExecutionConfig struct {
	// Framework to be used
	Framework string `json:"framework" yaml:"framework"`
	// OutputDir - path to the directory where generated files will be saved.
	OutputDir string `json:"output_dir" yaml:"output_dir"`
	URI       string `json:"uri,omitempty" yaml:"uri,omitempty"`
}

// ActionExecutionConfig represents the config required for creating an action.
type ActionExecutionConfig struct {
	// Kind of the action
	Kind string `json:"kind" yaml:"kind"`
	// The action's webhook URL
	HandlerWebhookBaseURL string `json:"handler_webhook_baseurl" yaml:"handler_webhook_baseurl"`
	// Config required to generate codegen
	Codegen *CodegenExecutionConfig `json:"codegen,omitempty" yaml:"codegen,omitempty"`
}

type Common struct {
	Actions     []Action    `json:"actions" yaml:"actions"`
	CustomTypes CustomTypes `json:"custom_types" yaml:"custom_types"`
}

func (c *Common) SetExportDefault() {
	for index := range c.Actions {
		c.Actions[index].Definition.Arguments = nil
		c.Actions[index].Definition.Type = ""
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

type ActionType string

const (
	// For query type
	ActionTypeQuery ActionType = "query"
	// For mutation type
	ActionTypeMutation = "mutation"
)

type ActionDef struct {
	Kind                 string          `json:"kind" yaml:"kind"`
	Type                 ActionType      `json:"type" yaml:"type,omitempty"`
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

// CLI Extension types

// DerivePayload defines the object required to derive operation
type DerivePayload struct {
	IntrospectionSchema interface{} `json:"introspection_schema" yaml:"introspection_schema,omitempty"`
	Operation           string      `json:"operation" yaml:"operation,omitempty"`
	ActionName          string      `json:"action_name" yaml:"action_name,omitempty"`
}

type SDLPayload struct {
	Complete string `json:"complete"`
}

type SDLToRequest struct {
	Types   CustomTypes   `json:"types,omitempty" yaml:"types,omitempty"`
	Actions []Action      `json:"actions,omitempty" yaml:"actions,omitempty"`
	Derive  DerivePayload `json:"derive,omitempty" yaml:"derive,omitempty"`
}

type SDLToResponse struct {
	SDL SDLPayload `json:"sdl"`
}

type SDLFromRequest struct {
	SDL SDLPayload `json:"sdl"`
}

type SDLFromResponse struct {
	Types   CustomTypes `json:"types"`
	Actions []Action    `json:"actions"`
}

type ActionsCodegenRequest struct {
	ActionName    string                  `json:"action_name" yaml:"action_name,omitempty"`
	SDL           SDLPayload              `json:"sdl" yaml:"sdl,omitempty"`
	Derive        DerivePayload           `json:"derive,omitempty"`
	CodegenConfig *CodegenExecutionConfig `json:"codegen_config" yaml:"codegen_config,omitempty"`
}

type CodegenFile struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

type ActionsCodegenResponse struct {
	Files []CodegenFile `json:"codegen" yaml:"codegen,omitempty"`
}
