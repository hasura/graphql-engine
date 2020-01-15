package types

type Metadata struct {
	Version          int         `json:"version,omitempty" yaml:"version,omitempty"`
	Tables           interface{} `json:"tables" yaml:"tables"`
	Functions        interface{} `json:"functions,omitempty" yaml:"functions"`
	QueryCollections interface{} `json:"query_collections,omitempty" yaml:"query_collections"`
	AllowList        interface{} `json:"allowlist,omitempty" yaml:"allowlist"`
	RemoteSchemas    interface{} `json:"remote_schemas,omitempty" yaml:"remote_schemas"`
	Actions          interface{} `json:"actions,omitempty" yaml:"actions"`
	CustomTypes      interface{} `json:"custom_types" yaml:"custom_types"`
}

type MetadataPlugins map[string]MetadataPluginsDriver

type MetadataPluginsDriver interface {
	Build(metadata *Metadata) error
	//TODO: create a tmp dir with the files, and then move the data
	Export(metadata Metadata) error
	CreateFiles() error
}
