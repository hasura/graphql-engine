package database

type MetadataDriver interface {
	ExportMetadata() (interface{}, error)

	ResetMetadata() error

	ReloadMetadata() error

	GetInconsistentMetadata() (bool, []InconsistentMetadataInterface, error)

	DropInconsistentMetadata() error

	ApplyMetadata(data interface{}) error

	Query(data []interface{}) error
}

type InconsistentMetadataInterface interface {
	GetType() string
	GetName() string
	GetDescription() string
	GetReason() string
}
