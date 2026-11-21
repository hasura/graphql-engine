package hasuradb

import (
	"fmt"
	"io"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (h *HasuraDB) ExportSchemaDump(
	includeSchemas []string,
	excludeSchemas []string,
	sourceName string,
	sourceKind hasura.SourceKind,
) ([]byte, error) {
	var op errors.Op = "hasuradb.HasuraDB.ExportSchemaDump"

	if sourceKind == hasura.SourceKindPG {
		opts := make([]string, 0, 3+2*len(includeSchemas)+2*len(excludeSchemas))
		opts = append(opts, "-O", "-x", "--schema-only")

		for _, s := range includeSchemas {
			opts = append(opts, "--schema", quoteSchemaName(s))
		}

		for _, s := range excludeSchemas {
			opts = append(opts, "--exclude-schema", quoteSchemaName(s))
		}

		query := hasura.PGDumpRequest{
			Opts:        opts,
			CleanOutput: true,
			SourceName:  sourceName,
		}

		resp, err := h.pgDumpClient.Send(query)
		if err != nil {
			h.logger.Debug(err)

			return nil, errors.E(op, err)
		}

		bs, err := io.ReadAll(resp)
		if err != nil {
			return nil, errors.E(
				op,
				errors.KindHasuraAPI,
				fmt.Errorf("reading response from schema dump api: %w", err),
			)
		}

		return bs, nil
	}

	return nil, errors.E(
		op,
		fmt.Errorf("schema dump for source %s of kind %v is not supported", sourceName, sourceKind),
	)
}

// quoteSchemaName wraps a schema name in double quotes before it is passed to
// pg_dump. pg_dump treats --schema values as patterns and folds unquoted names
// to lower case, so a schema called "TEST" could not be dumped otherwise.
// Names the user already quoted are passed through unchanged.
func quoteSchemaName(name string) string {
	if len(name) >= 2 && strings.HasPrefix(name, `"`) && strings.HasSuffix(name, `"`) {
		return name
	}

	return `"` + strings.ReplaceAll(name, `"`, `""`) + `"`
}
