package hasuradb

import (
	"net/http"
)

func (h *HasuraDB) ApplySeed(m interface{}) error {
	resp, body, err := h.sendv1Query(m)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
	}
	return nil
}

func (h *HasuraDB) ExportDataDump(fromTables []string) ([]byte, error) {
	pgDumpOpts := []string{"--no-owner", "--no-acl", "--data-only", "--column-inserts"}
	for _, table := range fromTables {
		pgDumpOpts = append(pgDumpOpts, "--table", table)
	}
	query := SchemaDump{
		Opts:        pgDumpOpts,
		CleanOutput: true,
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
