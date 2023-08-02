package cockroach

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

func (s *SourceOps) CockroachRunSQL(input hasura.CockroachRunSQLInput) (*hasura.CockroachRunSQLOutput, error) {
	var op errors.Op = "cockroach.SourceOps.CockroachRunSQL"
	body := hasura.RequestBody{
		Type: "cockroach_run_sql",
		Args: input,
	}
	var b = new(bytes.Buffer)
	resp, err := s.send(body, b)
	if err != nil {
		return nil, errors.E(op, err)
	}
	if resp.StatusCode != http.StatusOK {
		if b.Len() > 0 {
			return nil, errors.E(op, errors.KindHasuraAPI, b.String())
		} else {
			return nil, errors.E(op, errors.KindHasuraAPI, fmt.Errorf("cockroach_run_sql api request failed %d", resp.StatusCode))
		}
	}
	parsedResp := new(hasura.CockroachRunSQLOutput)
	if err = json.NewDecoder(b).Decode(parsedResp); err != nil {
		return nil, errors.E(op, err)
	}
	return parsedResp, nil
}
