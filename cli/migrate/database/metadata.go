package database

type MetadataDriver interface {
	SetMetadataPlugins(plugins interface{})

	ExportMetadata() error

	ResetMetadata() error

	ReloadMetadata() error

	GetInconsistentMetadata() (bool, []InconsistentMetadataInterface, error)

	DropInconsistentMetadata() error

	ApplyMetadata() error

	Query(data interface{}) error
}

type InconsistentMetadataInterface interface {
	GetType() string
	GetName() string
	GetDescription() string
	GetReason() string
}
