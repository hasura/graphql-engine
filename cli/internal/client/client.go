package client

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"sync"

	"github.com/pkg/errors"

	"github.com/hasura/graphql-engine/cli/version"

	"github.com/parnurzeal/gorequest"

	"github.com/mitchellh/mapstructure"
)

type APIVersion int

const (
	V1API = iota + 1
	V2API
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
	QueryURL    *url.URL
	MetadataURL *url.URL
	GraphqlURL  *url.URL
	PGDumpURL   *url.URL
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
func (c *HasuraRestAPIClient) CheckIfMigrationStateStoreWasMovedToCatalogState() (bool, error) {
	requestBody := strings.NewReader(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
		"sql": "SELECT COUNT(1) FROM information_schema.tables WHERE table_name = 'schema_migrations' AND table_schema = 'hdb_catalog' LIMIT 1"
	}
}
`))

	var response = struct {
		ResultType string     `json:"result_type"`
		Result     [][]string `json:"result"`
	}{}
	resp, respBody, err := c.SendQueryRequest(requestBody)
	if err != nil {
		return false, err
	}
	if resp.StatusCode != http.StatusOK {
		return false, errors.New(string(respBody))
	}
	if err := json.Unmarshal(respBody, &response); err != nil {
		return false, err
	}

	TuplesOK := "TuplesOk"
	if response.ResultType != TuplesOK {
		return false, fmt.Errorf("invalid result Type %s", response.ResultType)
	}

	if response.Result[1][0] == "0" {
		// table doesn't exist, so no need to anything
		return true, nil
	} else {
		// check if was migrated bit is set
		requestBody := strings.NewReader(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
		"sql": "SELECT version FROM hdb_catalog.schema_migrations WHERE version='-5'"
	}
}
`))

		var response = struct {
			ResultType string     `json:"result_type"`
			Result     [][]string `json:"result"`
		}{}
		resp, respBody, err := c.SendQueryRequest(requestBody)
		if err != nil {
			return false, err
		}
		if resp.StatusCode != http.StatusOK {
			return false, errors.New(string(respBody))
		}
		if err := json.Unmarshal(respBody, &response); err != nil {
			return false, err
		}

		TuplesOK := "TuplesOk"
		if response.ResultType != TuplesOK {
			return false, fmt.Errorf("invalid result Type %s", response.ResultType)
		}
		if len(response.Result) == 2 {
			return true, nil
		}
	}

	return false, nil
}

func (c *HasuraRestAPIClient) CheckIfSettingsStateWasMovedToCatalogState() (bool, error) {
	requestBody := strings.NewReader(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
		"sql": "SELECT COUNT(1) FROM information_schema.tables WHERE table_name = 'migration_settings' AND table_schema = 'hdb_catalog' LIMIT 1"
	}
}
`))

	var response = struct {
		ResultType string     `json:"result_type"`
		Result     [][]string `json:"result"`
	}{}
	resp, respBody, err := c.SendQueryRequest(requestBody)
	if err != nil {
		return false, err
	}
	if resp.StatusCode != http.StatusOK {
		return false, errors.New(string(respBody))
	}
	if err := json.Unmarshal(respBody, &response); err != nil {
		return false, err
	}

	TuplesOK := "TuplesOk"
	if response.ResultType != TuplesOK {
		return false, fmt.Errorf("invalid result Type %s", response.ResultType)
	}

	if response.Result[1][0] == "0" {
		return true, nil
	} else {
		// check if the migrated entry is set
		requestBody := strings.NewReader(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
			"sql": "SELECT value from hdb_catalog.migration_settings where setting='migrated_settings_to_catalog_state' LIMIT 1"
	}
}
`))

		var response = struct {
			ResultType string     `json:"result_type"`
			Result     [][]string `json:"result"`
		}{}
		resp, respBody, err := c.SendQueryRequest(requestBody)
		if err != nil {
			return false, err
		}
		if resp.StatusCode != http.StatusOK {
			return false, errors.New(string(respBody))
		}
		if err := json.Unmarshal(respBody, &response); err != nil {
			return false, err
		}

		TuplesOK := "TuplesOk"
		if response.ResultType != TuplesOK {
			return false, fmt.Errorf("invalid result Type %s", response.ResultType)
		}
		if len(response.Result) == 2 {
			return true, nil
		}
	}

	return false, nil
}

func (c *HasuraRestAPIClient) GetMigrationVersions(schemaName, tableName string) (map[uint64]bool, error) {
	requestBody := strings.NewReader(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
		"sql": "SELECT version, dirty FROM %s.%s"
	}
}
`, schemaName, tableName))

	var response = struct {
		ResultType string     `json:"result_type"`
		Result     [][]string `json:"result"`
	}{}
	resp, respBody, err := c.SendQueryRequest(requestBody)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, errors.New(string(respBody))
	}
	if err := json.Unmarshal(respBody, &response); err != nil {
		return nil, err
	}

	TuplesOK := "TuplesOk"
	if response.ResultType != TuplesOK {
		return nil, fmt.Errorf("invalid result Type %s", response.ResultType)
	}

	if len(response.Result) == 1 {
		return nil, nil
	}

	versions := map[uint64]bool{}
	for index, val := range response.Result {
		if index == 0 {
			continue
		}

		version, err := strconv.ParseUint(val[0], 10, 64)
		if err != nil {
			return nil, err
		}
		if len(val) == 2 {
			if val[1] == "f" {
				versions[version] = false
			}
			if val[1] == "t" {
				versions[version] = true
			}
		}

	}
	return versions, nil
}

type HasuraRestAPIClient struct {
	client         *http.Client
	headers        map[string]string
	queryAPIURL    *url.URL
	metadataAPIURL *url.URL
}

type NewHasuraRestAPIClientOpts struct {
	// headers which every request should have
	Headers        map[string]string
	QueryAPIURL    string
	MetadataAPIURL string
	TLSConfig      *tls.Config
}

func NewHasuraRestAPIClient(opts NewHasuraRestAPIClientOpts) (*HasuraRestAPIClient, error) {
	queryUrl, err := url.ParseRequestURI(opts.QueryAPIURL)
	if err != nil {
		return nil, err
	}
	metadataUrl, err := url.ParseRequestURI(opts.MetadataAPIURL)
	if err != nil {
		return nil, err
	}
	c := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: opts.TLSConfig,
		},
		CheckRedirect: nil,
		Timeout:       0,
	}
	v1client := &HasuraRestAPIClient{
		client:         c,
		headers:        opts.Headers,
		queryAPIURL:    queryUrl,
		metadataAPIURL: metadataUrl,
	}
	return v1client, nil
}

func (c *HasuraRestAPIClient) NewRequest(url string, body io.Reader) (*http.Request, error) {
	r, err := http.NewRequest(http.MethodPost, url, body)
	if err != nil {
		return nil, err
	}
	for k, v := range c.headers {
		r.Header.Set(k, v)
	}
	r.Header.Set("Content-Type", "application/json")
	return r, nil
}

func (c *HasuraRestAPIClient) Do(request *http.Request) (*http.Response, error) {
	return c.client.Do(request)
}

// send a hasura v1/query request
func (c *HasuraRestAPIClient) SendQueryRequest(body io.Reader) (*http.Response, []byte, error) {
	req, err := c.NewRequest(c.queryAPIURL.String(), body)
	if err != nil {
		return nil, nil, err
	}
	resp, err := c.Do(req)
	if err != nil {
		return nil, nil, err
	}
	defer resp.Body.Close()
	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, nil, err
	}
	return resp, b, nil
}

// send a hasura v1/query request
func (c *HasuraRestAPIClient) SendV1MetadataRequest(body io.Reader) (*http.Response, []byte, error) {
	req, err := c.NewRequest(c.metadataAPIURL.String(), body)
	if err != nil {
		return nil, nil, err
	}
	resp, err := c.Do(req)
	if err != nil {
		return nil, nil, err
	}
	defer resp.Body.Close()
	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, nil, err
	}
	return resp, b, nil
}
func (c *HasuraRestAPIClient) GetCLISettingsFromSQLTable(schemaName string, tableName string) (map[string]string, error) {
	requestBody := strings.NewReader(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
		"sql": "SELECT setting, value FROM %s.%s"
	}
}`, schemaName, tableName))

	var response = struct {
		ResultType string     `json:"result_type"`
		Result     [][]string `json:"result"`
	}{}
	resp, respBody, err := c.SendQueryRequest(requestBody)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, errors.New(string(respBody))
	}
	if err := json.Unmarshal(respBody, &response); err != nil {
		return nil, err
	}

	TuplesOK := "TuplesOk"
	if response.ResultType != TuplesOK {
		return nil, fmt.Errorf("invalid result Type %s", response.ResultType)
	}

	if len(response.Result) == 1 {
		return nil, nil
	}

	settings := map[string]string{}
	for index, val := range response.Result {
		if index == 0 {
			continue
		}
		if len(val) == 2 {
			settings[val[0]] = settings[val[1]]
		}
	}
	return settings, nil
}

func (c *HasuraRestAPIClient) MarkCLIStateTablesAsMovedToCatalogState() error {
	requestBody := bytes.NewReader([]byte(fmt.Sprintf(`
{
	"type": "run_sql",
	"args":{
		"sql": "INSERT INTO hdb_catalog.migration_settings (setting, value) VALUES ('migrated_settings_to_catalog_state', 'true'); INSERT INTO hdb_catalog.schema_migrations  (version, dirty) VALUES ('-5', 'f');"
	}
}
`)))

	resp, respBody, err := c.SendQueryRequest(requestBody)
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		return errors.New(string(respBody))
	}
	return nil
}
func (c *HasuraRestAPIClient) MoveMigrationsAndSettingsToCatalogState(datasource string, migrations map[uint64]bool, settings map[string]string) error {
	catalogStateMigrations := MigrationsState{}
	catalogStateMigrations[datasource] = map[string]bool{}
	for k, v := range migrations {
		catalogStateMigrations[datasource][strconv.Itoa(int(k))] = v
	}
	cliState := CLICatalogState{
		Migrations: catalogStateMigrations,
		Settings:   settings,
	}
	type args struct {
		Type  string          `json:"type"`
		State CLICatalogState `json:"state"`
	}
	request := struct {
		Type string `json:"type"`
		Args args   `json:"args"`
	}{
		Type: "set_catalog_state",
		Args: args{
			Type:  "cli",
			State: cliState,
		},
	}
	b, err := json.Marshal(request)
	if err != nil {
		return err
	}
	resp, body, err := c.SendV1MetadataRequest(bytes.NewReader(b))
	if err != nil {
		return err
	}
	if resp.StatusCode != http.StatusOK {
		return errors.New(string(body))
	}

	return nil
}

func (c *HasuraRestAPIClient) GetDatasources() (map[string]string, error) {
	request := struct {
		Type string                 `json:"type"`
		Args map[string]interface{} `json:"args"`
	}{
		Type: "export_metadata",
		Args: map[string]interface{}{},
	}

	b, err := json.Marshal(request)
	if err != nil {
		return nil, err
	}
	resp, body, err := c.SendV1MetadataRequest(bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, errors.New(string(body))
	}

	var bodyAsMap = map[string]interface{}{}
	if err = json.Unmarshal(body, &bodyAsMap); err != nil {
		return nil, errors.Wrap(err, "unmarshalling response from API")
	}

	sources, ok := bodyAsMap["sources"]
	if !ok {
		return nil, fmt.Errorf("no sources found")
	}

	var sourcesList []map[string]interface{}
	if err := mapstructure.Decode(sources, &sourcesList); err != nil {
		return nil, errors.Wrap(err, "error unmarshalling sources list")
	}
	// name: type
	datasourcesList := map[string]string{}
	fmt.Println(sourcesList)
	for _, source := range sourcesList {
		sourceNameValue, ok := source["name"]
		if !ok {
			return nil, fmt.Errorf("error getting source name")
		}
		sourceName, ok := sourceNameValue.(string)
		if !ok {
			return nil, fmt.Errorf("error getting source name")
		}
		//sourceTypeValue, ok := source["type"]
		//if !ok {
		//	return nil, fmt.Errorf("error getting source type")
		//}
		//sourceType, ok := sourceTypeValue.(string)
		//if !ok {
		//	return nil, fmt.Errorf("error getting source type")
		//}
		// TODO: fill in source type too
		datasourcesList[sourceName] = ""
	}
	return datasourcesList, nil
}
