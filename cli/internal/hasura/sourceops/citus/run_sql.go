package citus

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (d *SourceOps) CitusRunSQL(input hasura.CitusRunSQLInput) (*hasura.CitusRunSQLOutput, error) {
	var op errors.Op = "citus.SourceOps.CitusRunSQL"
	body := hasura.RequestBody{
		Type: "citus_run_sql",
		Args: input,
	}
	var b = new(bytes.Buffer)
	resp, err := d.send(body, b)
	if err != nil {
		return nil, errors.E(op, err)
	}
	if resp.StatusCode != http.StatusOK {
		if b.Len() > 0 {
			return nil, errors.E(op, errors.KindHasuraAPI, b.String())
		} else {
			return nil, errors.E(op, errors.KindHasuraAPI, fmt.Errorf("citus_run_sql api request failed %d", resp.StatusCode))
		}
	}
	o := new(hasura.CitusRunSQLOutput)
	if err = json.NewDecoder(b).Decode(o); err != nil {
		return nil, errors.E(op, err)
	}
	return o, nil
}
