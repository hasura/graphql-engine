package database

import "github.com/hasura/graphql-engine/cli/v2/pkg/hasura"

type SchemaDriver interface {
	ExportSchemaDump(includeSchemas []string, excludeSchemas []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error)
}
