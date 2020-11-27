package client

import (
	"fmt"
	"net/http"
	nurl "net/url"
	"sync"

	"github.com/hasura/graphql-engine/cli/version"

	"github.com/parnurzeal/gorequest"

	"github.com/mitchellh/mapstructure"
)

var (
	queryTypes    = []string{"select", "insert", "select", "update", "delete", "count", "run_sql", "bulk"}
	queryTypesMap = func() map[string]bool {
		var m = map[string]bool{}
		for _, v := range queryTypes {
			m[v] = true
		}
		return m
	}()
)

type QueryRequestOpts struct{}

type MetadataRequestOpts struct{}

type MetadataOrQueryClientFuncOpts struct {
	QueryRequestOpts    *QueryRequestOpts
	MetadataRequestOpts *MetadataRequestOpts
}
type MetadataOrQueryClientFunc func(m interface{}, opts MetadataOrQueryClientFuncOpts, config Config) (*http.Response, []byte, error)

type Config struct {
	QueryURL    *nurl.URL
	MetadataURL *nurl.URL
	GraphqlURL  *nurl.URL
	PGDumpURL   *nurl.URL
	Req         *gorequest.SuperAgent
	Headers     map[string]string
}
type Client struct {
	mutex              sync.Mutex
	serverFeatureFlags *version.ServerFeatureFlags
}

func NewClient(flags *version.ServerFeatureFlags) *Client {
	return &Client{serverFeatureFlags: flags}
}

func (c *Client) Sendv1Query(m interface{}, _ MetadataOrQueryClientFuncOpts, config Config) (resp *http.Response, body []byte, err error) {
	c.mutex.Lock()
	defer c.mutex.Unlock()

	request := config.Req.Clone()
	request = request.Post(config.QueryURL.String()).Send(m)
	for headerName, headerValue := range config.Headers {
		request.Set(headerName, headerValue)
	}

	resp, body, errs := request.EndBytes()

	if len(errs) == 0 {
		err = nil
	} else {
		err = errs[0]
	}

	return resp, body, err
}

func (c *Client) SendV2QueryOrV1Metadata(m interface{}, opts MetadataOrQueryClientFuncOpts, config Config) (resp *http.Response, body []byte, err error) {
	c.mutex.Lock()
	defer c.mutex.Unlock()

	var endpoint string
	switch {
	case !c.serverFeatureFlags.HasDatasources:
		endpoint = config.QueryURL.String()
	case c.serverFeatureFlags.HasDatasources:
		// TODO: Make this better
		if opts.MetadataRequestOpts != nil {
			endpoint = config.MetadataURL.String()
			break
		}
		if opts.QueryRequestOpts != nil {
			endpoint = config.QueryURL.String()
			break
		}
		if endpoint == "" {
			var v map[string]interface{}
			if err := mapstructure.Decode(m, &v); err != nil {
				return nil, nil, fmt.Errorf("unmarshalling request body failed")
			} else {
				requestType := fmt.Sprintf("%v", v["Type"])
				if _, ok := queryTypesMap[requestType]; ok {
					endpoint = config.QueryURL.String()
				} else {
					endpoint = config.MetadataURL.String()
				}
			}
		}

	}

	request := config.Req.Clone()
	request = request.Post(endpoint).Send(m)
	for headerName, headerValue := range config.Headers {
		request.Set(headerName, headerValue)
	}

	resp, body, errs := request.EndBytes()

	if len(errs) == 0 {
		err = nil
	} else {
		err = errs[0]
	}

	return resp, body, err
}
