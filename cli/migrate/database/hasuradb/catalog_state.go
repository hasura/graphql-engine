package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/mitchellh/mapstructure"

	"github.com/pkg/errors"
)

type MigrationsState []DatasourceState

type DatasourceState struct {
	Name     string
	Versions map[string]bool `json:"versions"`
}

type SettingsState struct {
}

type CLICatalogState struct {
	Migrations    MigrationsState
	SettingsState SettingsState
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

func (c *CatalogStateAPI) GetCLICatalogState(hasuradb *HasuraDB) (*CLICatalogState, error) {
	// useful for construcing errors
	var opName = "getting catalog state"
	q := HasuraInterfaceQuery{
		Type: "get_catalog_state",
		Args: HasuraArgs{},
	}
	resp, body, err := hasuradb.sendQueryOrMetadataRequest(q)
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
	if err := mapstructure.Decode(v, cliState); err != nil {
		return nil, errors.Wrap(err, opName)
	}
	return cliState, nil

}

func (c *CatalogStateAPI) SetCLICatalogState(hasuradb *HasuraDB, cliState CLICatalogState) error {
	// useful for construcing errors
	var opName = "setting catalog state"

	q := HasuraInterfaceQuery{
		Type: "set_catalog_state",
		Args: CatalogState{
			CLIState: cliState,
		},
	}
	resp, body, err := hasuradb.sendQueryOrMetadataRequest(q)
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
