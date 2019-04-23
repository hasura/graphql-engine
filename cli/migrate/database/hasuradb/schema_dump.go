package hasuradb

import (
	"encoding/json"
	"net/http"
)

func (h *HasuraDB) ExportSchemaDump(schemaNames []string) ([]byte, error) {
	opts := []string{"-O", "-x", "--schema-only"}
	for _, s := range schemaNames {
		opts = append(opts, "--schema", s)
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return nil, err
		}
		return nil, horror.Error(h.config.isCMD)
	}

	return body, nil
}
