package database

type MetadataDriver interface {
	ExportMetadata() (interface{}, error)

	ResetMetadata() error

	ApplyMetadata(data interface{}) error

	Query(data []interface{}) error
}
