package metadata

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"path"

	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	"github.com/parnurzeal/gorequest"
	"github.com/pkg/errors"
)

const (
	v1Query string = "/v1/query"
)

type hasuraDBConfig struct {
	endpoint *url.URL

	accessSecret string
}

func newhasuraDB(endpoint string, accessSecret string) (*hasuraDBConfig, error) {
	nurl, err := url.Parse(endpoint)
	if err != nil {
		return nil, errors.Wrap(err, "cannot parse endpoint")
	}
	nurl.Path = path.Join(nurl.Path, v1Query)
	return &hasuraDBConfig{
		endpoint:     nurl,
		accessSecret: accessSecret,
	}, nil
}

func (h *hasuraDBConfig) sendQuery(query interface{}) ([]byte, error) {
	request := gorequest.New()
	resp, body, errs := request.Post(h.endpoint.String()).
		Set("X-Hasura-Admin-Secret", h.accessSecret).
		Send(query).
		EndBytes()

	if resp.StatusCode != http.StatusOK {
		var herror hasuradb.HasuraError
		err := json.Unmarshal(body, &herror)
		if err != nil {
			return nil, err
		}

		return nil, herror.Error(true)
	}
	if len(errs) != 0 {
		return nil, fmt.Errorf("%v", errs)
	}
	return body, nil
}
