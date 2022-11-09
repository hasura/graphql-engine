package hasuradb

import (
	"fmt"
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (h *HasuraDB) ExportSchemaDump(includeSchemas []string, excludeSchemas []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
	var op errors.Op = "hasuradb.HasuraDB.ExportSchemaDump"
	switch sourceKind {
	case hasura.SourceKindPG:
		opts := []string{"-O", "-x", "--schema-only"}
		for _, s := range includeSchemas {
			opts = append(opts, "--schema", s)
		}
		for _, s := range excludeSchemas {
			opts = append(opts, "--exclude-schema", s)
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
		bs, err := ioutil.ReadAll(resp)
		if err != nil {
			return nil, errors.E(op, errors.KindHasuraAPI, fmt.Errorf("reading response from schema dump api: %w", err))
		}
		return bs, nil
	}
	return nil, errors.E(op, fmt.Errorf("schema dump for source %s of kind %v is not supported", sourceName, sourceKind))
}
