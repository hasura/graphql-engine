package database

type SchemaDriver interface {
	ExportSchemaDump(schemaName []string) ([]byte, error)
}
