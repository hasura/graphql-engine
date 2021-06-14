package database

import "github.com/hasura/graphql-engine/cli/internal/hasura"

type SchemaDriver interface {
	ExportSchemaDump(schemaName []string, sourceName string, sourceKind hasura.SourceKind,  excludeSchema []string, full bool) ([]byte, error)
}
