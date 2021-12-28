package database

import "github.com/hasura/graphql-engine/cli/v2/internal/hasura"

type SchemaDriver interface {
	ExportSchemaDump(schemaName []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error)
}
