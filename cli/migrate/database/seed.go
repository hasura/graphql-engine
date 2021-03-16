package database

import "github.com/hasura/graphql-engine/cli/internal/hasura"

type SeedDriver interface {
	ApplySeed(m interface{}) error
	ExportDataDump(tableNames []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error)
}
