package hasuradb

import (
	"net/http"
)

// ExportSchemaDump calls pg_dump to help initialize the first set of migrations
func (h *HasuraDB) ExportSchemaDump(schemaNames, excludeSchema []string, full bool) ([]byte, error) {
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
