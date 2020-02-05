package database

import "github.com/hasura/graphql-engine/cli/metadata/types"

type MetadataDriver interface {
	SetMetadataPlugins(plugins interface{})

	ExportMetadata() (map[string][]byte, error)

	ResetMetadata() error

	ReloadMetadata() error

	GetInconsistentMetadata() (bool, []InconsistentMetadataInterface, error)

	DropInconsistentMetadata() error

	BuildMetadata() (types.Metadata, error)

	ApplyMetadata() error

	Query(data interface{}) error
}

type InconsistentMetadataInterface interface {
	GetType() string
	GetName() string
	GetDescription() string
	GetReason() string
}
