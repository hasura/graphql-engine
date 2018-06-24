package database

type Setting struct {
	name         string
	defaultValue string
}

var Settings = []Setting{
	{
		name:         "migration_mode",
		defaultValue: "true",
	},
}

type SettingsDriver interface {
	// Get Current setting from database
	GetSetting(name string) (value string, err error)

	// UpdateSetting updates a setting in database.
	UpdateSetting(name string, value string) error
}

func (s *Setting) GetName() string {
	return s.name
}

func (s *Setting) GetDefaultValue() string {
	return s.defaultValue
}
