package hasuradb

import (
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/hasura"
)

// ExportSchemaDump calls pg_dump to help initialize the first set of migrations
func (h *HasuraDB) ExportSchemaDump(schemaNames []string, sourceName string, sourceKind hasura.SourceKind, excludeSchema []string, full bool) ([]byte, error) {
	switch sourceKind {
	case hasura.SourceKindPG:
		opts := []string{"-O", "-x", "--schema-only"}
    if len(excludeSchema) != 0 {
		for _, s := range excludeSchema {
			opts = append(opts, "--exclude-schema", s)
		}
	}
	if !full {
		for _, s := range schemaNames {
			opts = append(opts, "--schema", s)
		}
	}
		query := SchemaDump{
			Opts:        opts,
			CleanOutput: true,
			Database:    sourceName,
		}

		resp, body, err := h.sendSchemaDumpQuery(query)
		if err != nil {
			h.logger.Debug(err)
			return nil, err
		}
		h.logger.Debug("response: ", string(body))

		if resp.StatusCode != http.StatusOK {
			return nil, NewHasuraError(body, h.config.isCMD)
		}

		return body, nil
	}
	return nil, fmt.Errorf("schema dump for source %s of kind %v is not supported", sourceName, sourceKind)
}
