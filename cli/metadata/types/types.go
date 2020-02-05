package types

import (
	"gopkg.in/yaml.v2"
)

type Metadata struct {
	Version          int           `json:"version,omitempty" yaml:"version,omitempty"`
	Tables           interface{}   `json:"tables" yaml:"tables"`
	Functions        []interface{} `json:"functions,omitempty" yaml:"functions,omitempty"`
	QueryCollections []interface{} `json:"query_collections,omitempty" yaml:"query_collections,omitempty"`
	AllowList        []interface{} `json:"allowlist,omitempty" yaml:"allowlist,omitempty"`
	RemoteSchemas    []interface{} `json:"remote_schemas,omitempty" yaml:"remote_schemas,omitempty"`
	Actions          interface{}   `json:"actions,omitempty" yaml:"actions,omitempty"`
	CustomTypes      interface{}   `json:"custom_types" yaml:"custom_types,omitempty"`
}

type MetadataPlugins map[string]MetadataPluginsDriver

type MetadataPluginsDriver interface {
	Build(metadata *Metadata) error
	Export(metadata yaml.MapSlice) (map[string][]byte, error)
	CreateFiles() error
}
