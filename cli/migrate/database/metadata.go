package database

type MetadataDriver interface {
	EnableCheckMetadataConsistency(bool)
}
