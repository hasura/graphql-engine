package types

import (
	actionTypes "github.com/hasura/graphql-engine/cli/metadata/actions/types"
)

type Metadata struct {
	Version          string                   `json:"version" yaml:"version"`
	Tables           []map[string]interface{} `json:"tables" yaml:"tables"`
	Functions        []map[string]interface{} `json:"functions,omitempty" yaml:"functions"`
	QueryCollections []map[string]interface{} `json:"query_collections,omitempty" yaml:"query_collections"`
	AllowList        []map[string]interface{} `json:"allowlist,omitempty" yaml:"allowlist"`
	RemoteSchemas    []map[string]interface{} `json:"remote_schemas,omitempty" yaml:"remote_schemas"`
	Actions          []actionTypes.Action     `json:"actions,omitempty" yaml:"actions"`
	CustomTypes      actionTypes.CustomTypes  `json:"custom_types" yaml:"custom_types"`
}

type MetadataPlugins map[string]MetadataPluginsDriver

type MetadataPluginsDriver interface {
	Build(metadata *Metadata) error
	// Should create a tmp dir with the files, and then move the data
	Export(metadata Metadata) error
}
