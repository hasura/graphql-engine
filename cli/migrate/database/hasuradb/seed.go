package hasuradb

import (
	"bytes"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/pkg/errors"
)

func (h *HasuraDB) ApplySeed(m interface{}) error {
	resp, body, err := h.genericQueryRequest(m)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		v, ok := body.(*bytes.Buffer)
		if ok {
			return errors.New(v.String())
		}
		return fmt.Errorf("applying %v failed with code %d", m, resp.StatusCode)
	}
	return nil
}

func (h *HasuraDB) ExportDataDump(fromTables []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
	switch sourceKind {
	case hasura.SourceKindPG:
		pgDumpOpts := []string{"--no-owner", "--no-acl", "--data-only", "--column-inserts"}
		for _, table := range fromTables {
			pgDumpOpts = append(pgDumpOpts, "--table", table)
		}
		query := SchemaDump{
			Opts:        pgDumpOpts,
			CleanOutput: true,
			Database:    sourceName,
		}

		resp, body, err := h.sendSchemaDumpQuery(query)
		if err != nil {
			h.logger.Debug(err)
			return nil, err
		}
		h.logger.Debug("exporting data: ", string(body))

		if resp.StatusCode != http.StatusOK {
			return nil, NewHasuraError(body, h.config.isCMD)
		}

		return body, nil
	}
	return nil, fmt.Errorf("not supported for source %s of type %v", sourceName, sourceKind)
}
