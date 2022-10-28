package postgres

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (d *SourceOps) PGRunSQL(input hasura.PGRunSQLInput) (*hasura.PGRunSQLOutput, error) {
	var op errors.Op = "postgres.SourceOps.PGRunSQL"
	body := hasura.RequestBody{
		Type: "run_sql",
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
			return nil, errors.E(op, errors.KindHasuraAPI, fmt.Errorf("run_sql api request failed %d", resp.StatusCode))
		}
	}
	o := new(hasura.PGRunSQLOutput)
	if err = json.NewDecoder(b).Decode(o); err != nil {
		return nil, errors.E(op, err)
	}
	return o, nil
}
