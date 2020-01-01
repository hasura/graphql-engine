package database

type SchemaDriver interface {
	ExportSchemaDump(schemaNames []string, includeData bool) ([]byte, error)
	ExportDataDump(schemaNames []string, tableNames []string) ([]byte, error)
}
