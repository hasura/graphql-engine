package database

type MetadataDriver interface {
	SetMetadataPlugins(plugins interface{})

	ExportMetadata() (interface{}, error)

	ResetMetadata() error

	ReloadMetadata() error

	ApplyMetadata(data interface{}) error

	Query(data []interface{}) error
}
