package hasuradb

import (
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func (h *HasuraDB) GetSetting(name string) (value string, err error) {
	var op errors.Op = "hasuradb.HasuraDB.GetSetting"
	value, err = h.settingsStateStore.GetSetting(name)
	if err != nil {
		return value, errors.E(op, err)
	}
	return value, nil
}

func (h *HasuraDB) UpdateSetting(name string, value string) error {
	var op errors.Op = "hasuradb.HasuraDB.UpdateSetting"
	err := h.settingsStateStore.UpdateSetting(name, value)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
