package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"
)

func (h *HasuraDB) ExportSchemaDump(schemaName string) ([]byte, error) {
	query := SchemaDump{
		Opts:  fmt.Sprintf("-O -x --schema %s --schema-only", schemaName),
		Clean: true,
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
