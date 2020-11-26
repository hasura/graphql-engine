package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/migrate/database"

	"github.com/mitchellh/mapstructure"

	"github.com/pkg/errors"
)

type CatalogStateAPIClient interface {
	sendMetadataOrQueryRequest(m interface{}, queryType string) (*http.Response, []byte, error)
}

type MigrationsState map[database.Datasource]map[string]bool

type SettingsState struct {
	MigrationMode *bool `json:"migrationMode"`
}

type CLICatalogState struct {
	Migrations MigrationsState   `json:"migrations" mapstructure:"migrations"`
	Settings   map[string]string `json:"settings" mapstructure:"settings"`
}

type CatalogState struct {
	ID           *string         `json:"id,omitempty" mapstructure:"id,omitempty"`
	ConsoleState interface{}     `json:"console_state,omitempty" mapstructure:"console_state,omitempty"`
	CLIState     CLICatalogState `json:"cli_state,omitempty" mapstructure:"cli_state,omitempty"`
}

type CatalogStateAPI struct {
	// key name in which cli state is stored
	CLIStateKeyName string
}

const defaultCLIStateKey = "cli_state"

func NewCatalogStateAPI(cliStateKey string) *CatalogStateAPI {
	return &CatalogStateAPI{
		CLIStateKeyName: cliStateKey,
	}
}

func (c *CatalogStateAPI) GetCLICatalogState(client CatalogStateAPIClient) (*CLICatalogState, error) {
	// useful for construcing errors
	var opName = "getting catalog state"
	q := HasuraInterfaceQuery{
		Type: "get_catalog_state",
		Args: HasuraArgs{},
	}
	resp, body, err := client.sendMetadataOrQueryRequest(q, "")
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		if s := string(body); s != "" {
			return nil, fmt.Errorf("%s: %s", opName, s)
		}
		return nil, fmt.Errorf("%s failed", opName)
	}
	var state map[string]interface{}
	if err := json.Unmarshal(body, &state); err != nil {
		return nil, errors.Wrap(err, opName)
	}
	v, ok := state[c.CLIStateKeyName]
	if !ok {
		return nil, fmt.Errorf("%v: %s", opName, "cli state no found")
	}

	var cliState = new(CLICatalogState)
	fmt.Printf("%+v\n", state)
	if err := mapstructure.Decode(v, cliState); err != nil {
		return nil, errors.Wrap(err, opName)
	}
	fmt.Printf("%+v\n", cliState)
	return cliState, nil

}

func (c *CatalogStateAPI) SetCLICatalogState(client CatalogStateAPIClient, cliState CLICatalogState) error {
	// useful for constructing errors
	var opName = "setting catalog state"

	q := HasuraInterfaceQuery{
		Type: "set_catalog_state",
		Args: struct {
			Type  string          `json:"type"`
			State CLICatalogState `json:"state"`
		}{
			Type:  "cli",
			State: cliState,
		},
	}
	resp, body, err := client.sendMetadataOrQueryRequest(q, "")
	if err != nil {
		return err
	}

	if resp.StatusCode != http.StatusOK {
		if s := string(body); s != "" {
			return fmt.Errorf("%s: %s", opName, s)
		}
		return fmt.Errorf("%s failed", opName)
	}
	return nil
}
