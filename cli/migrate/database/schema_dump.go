package database

type SchemaDriver interface {
	ExportSchemaDump(schemaName []string, excludeSchema bool) ([]byte, error)
}
