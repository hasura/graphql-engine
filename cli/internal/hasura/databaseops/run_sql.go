package databaseops

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/internal/hasura"
)

func (h *ClientDatabaseOps) RunSQL(input hasura.RunSQLInput) (*hasura.RunSQLOutput, error) {
	body := hasura.RequestBody{
		Type: "run_sql",
		Args: input,
	}
	var b = new(bytes.Buffer)
	resp, err := h.send(body, b)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		if b.Len() > 0 {
			return nil, fmt.Errorf(b.String())
		} else {
			return nil, fmt.Errorf("run_sql api request failed %d", resp.StatusCode)
		}
	}
	o := new(hasura.RunSQLOutput)
	if err = json.NewDecoder(b).Decode(o); err != nil {
		return nil, err
	}
	return o, nil
}
