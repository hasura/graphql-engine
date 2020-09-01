package hasuradb

import (
	"net/http"
)

// ExportSchemaDump calls pg_dump to help initialize the first set of migrations
func (h *HasuraDB) ExportSchemaDump(schemaNames []string, excludeSchema bool) ([]byte, error) {
	opts := []string{"-O", "-x", "--schema-only"}
	schemaOption := "--schema"
	if excludeSchema {
		schemaOption = "--exclude-schema"
	}
	for _, s := range schemaNames {
		opts = append(opts, schemaOption, s)
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
