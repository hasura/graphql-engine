package settings

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

func (s *Setting) GetName() string {
	return s.name
}

func (s *Setting) GetDefaultValue() string {
	return s.defaultValue
}
