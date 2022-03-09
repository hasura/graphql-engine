package hasuradb

import (
	"fmt"
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (h *HasuraDB) ExportSchemaDump(schemaNames []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
	switch sourceKind {
	case hasura.SourceKindPG:
		opts := []string{"-O", "-x", "--schema-only"}
		for _, s := range schemaNames {
			opts = append(opts, "--schema", s)
		}
		query := hasura.PGDumpRequest{
			Opts:        opts,
			CleanOutput: true,
			SourceName:  sourceName,
		}

		resp, err := h.pgDumpClient.Send(query)
		if err != nil {
			h.logger.Debug(err)
			return nil, err
		}
		bs, err := ioutil.ReadAll(resp)
		if err != nil {
			return nil, fmt.Errorf("reading response from schema dump api: %w", err)
		}
		return bs, nil
	}
	return nil, fmt.Errorf("schema dump for source %s of kind %v is not supported", sourceName, sourceKind)
}
