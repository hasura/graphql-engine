package database

type SchemaDriver interface {
	ExportSchemaDump(schemaName []string, database string) ([]byte, error)
}
