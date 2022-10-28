package settings

import (
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/statestore"
)

type StateStoreCatalog struct {
	client *statestore.CLICatalogState
}

func NewStateStoreCatalog(c *statestore.CLICatalogState) *StateStoreCatalog {
	return &StateStoreCatalog{c}
}

func (s StateStoreCatalog) GetSetting(key string) (value string, err error) {
	var op errors.Op = "settings.StateStoreCatalog.GetSetting"
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return "", errors.E(op, err)
	}
	return state.GetSetting(key), nil
}

func (s StateStoreCatalog) UpdateSetting(name string, value string) error {
	var op errors.Op = "settings.StateStoreCatalog.UpdateSetting"
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return errors.E(op, err)
	}
	state.SetSetting(name, value)
	_, err = s.client.Set(*state)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (s StateStoreCatalog) GetAllSettings() (map[string]string, error) {
	var op errors.Op = "settings.StateStoreCatalog.GetAllSettings"
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return nil, errors.E(op, err)
	}
	return state.GetSettings(), nil
}

func (s StateStoreCatalog) PrepareSettingsDriver() error {
	var op errors.Op = "settings.StateStoreCatalog.PrepareSettingsDriver"
	if err := s.setDefaults(); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (s StateStoreCatalog) setDefaults() error {
	var op errors.Op = "settings.StateStoreCatalog.setDefaults"
	// get setting
	state, err := s.client.Get()
	if err != nil {
		return errors.E(op, err)
	}
	for _, setting := range Settings {
		if v := state.GetSetting(setting.GetName()); len(v) == 0 {
			state.SetSetting(setting.GetName(), setting.GetDefaultValue())
		}
	}
	_, err = s.client.Set(*state)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
