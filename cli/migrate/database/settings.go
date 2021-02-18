package database

type SettingsDriver interface {
	// Get Current setting from database
	GetSetting(name string) (value string, err error)

	// UpdateSetting updates a setting in database.
	UpdateSetting(name string, value string) error
}
