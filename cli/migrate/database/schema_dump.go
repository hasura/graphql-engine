package database

type SchemaDriver interface {
	ExportSchemaDump(schemaName, excludeSchema []string, full bool) ([]byte, error)
}
