package database

import (
	"github.com/hasura/graphql-engine/cli/metadata/types"
	"gopkg.in/yaml.v2"
)

type MetadataDriver interface {
	SetMetadataPlugins(types.MetadataPlugins)

	ExportMetadata() (map[string][]byte, error)

	ResetMetadata() error

	ReloadMetadata() error

	GetInconsistentMetadata() (bool, []InconsistentMetadataInterface, error)

	DropInconsistentMetadata() error

	BuildMetadata() (yaml.MapSlice, error)

	ApplyMetadata() error

	Query(data interface{}) error
}

type InconsistentMetadataInterface interface {
	GetType() string
	GetName() string
	GetDescription() string
	GetReason() string
}
