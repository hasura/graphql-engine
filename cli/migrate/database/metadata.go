package database

type MetadataDriver interface {
	SetMetadataPlugins(plugins interface{})

	ExportMetadata() error

	ResetMetadata() error

	ReloadMetadata() error

	ApplyMetadata() error

	Query(data interface{}) error
}
