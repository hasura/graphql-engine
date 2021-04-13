package hasuradb

func (h *HasuraDB) GetSetting(name string) (value string, err error) {
	return h.settingsStateStore.GetSetting(name)
}

func (h *HasuraDB) UpdateSetting(name string, value string) error {
	return h.settingsStateStore.UpdateSetting(name, value)
}
