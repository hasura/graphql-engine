package citus

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (d *SourceOps) CitusRunSQL(input hasura.CitusRunSQLInput) (*hasura.CitusRunSQLOutput, error) {
	body := hasura.RequestBody{
		Type: "citus_run_sql",
		Args: input,
	}
	var b = new(bytes.Buffer)
	resp, err := d.send(body, b)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		if b.Len() > 0 {
			return nil, fmt.Errorf(b.String())
		} else {
			return nil, fmt.Errorf("citus_run_sql api request failed %d", resp.StatusCode)
		}
	}
	o := new(hasura.CitusRunSQLOutput)
	if err = json.NewDecoder(b).Decode(o); err != nil {
		return nil, err
	}
	return o, nil
}
