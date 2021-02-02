package database

type SeedDriver interface {
	ApplySeed(m interface{}) error
	ExportDataDump(tableNames []string) ([]byte, error)
}
