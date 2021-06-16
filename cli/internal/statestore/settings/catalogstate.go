package settings

import (
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"
)

type StateStoreCatalog struct {
	client *statestore.CLICatalogState
}

func NewStateStoreCatalog(c *statestore.CLICatalogState) *StateStoreCatalog {
	return &StateStoreCatalog{c}
}

func (s StateStoreCatalog) GetSetting(key string) (value string, err error) {
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return "", err
	}
	return state.GetSetting(key), nil
}

func (s StateStoreCatalog) UpdateSetting(name string, value string) error {
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return err
	}
	state.SetSetting(name, value)
	_, err = s.client.Set(*state)
	if err != nil {
		return err
	}
	return nil
}

func (s StateStoreCatalog) GetAllSettings() (map[string]string, error) {
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return nil, err
	}
	return state.GetSettings(), nil
}

func (s StateStoreCatalog) PrepareSettingsDriver() error {
	return s.setDefaults()
}

func (s StateStoreCatalog) setDefaults() error {
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return err
	}
	for _, setting := range Settings {
		if v := state.GetSetting(setting.GetName()); len(v) == 0 {
			state.SetSetting(setting.GetName(), setting.GetDefaultValue())
		}
	}
	_, err = s.client.Set(*state)
	if err != nil {
		return err
	}
	return nil
}
