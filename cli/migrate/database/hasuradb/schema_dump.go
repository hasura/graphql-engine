package hasuradb

import (
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (h *HasuraDB) ExportSchemaDump(schemaNames []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
	switch sourceKind {
	case hasura.SourceKindPG:
		opts := []string{"-O", "-x", "--schema-only"}
		for _, s := range schemaNames {
			opts = append(opts, "--schema", s)
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
