package hasura

const (
	CommandOK RunSQLResultType = "CommandOk"
	TuplesOK  RunSQLResultType = "TuplesOk"
)

// hasura API requests used to interact with pg sources
type PGSourceOps interface {
	PGRunSQL(input PGRunSQLInput) (response *PGRunSQLOutput, err error)
}

type MSSQLSourceOps interface {
	MSSQLRunSQL(input MSSQLRunSQLInput) (response *MSSQLRunSQLOutput, err error)
}

type PGRunSQLInput struct {
	SQL                      string `json:"sql" yaml:"sql"`
	Source                   string `json:"source,omitempty" yaml:"source,omitempty"`
	Cascade                  bool   `json:"cascade,omitempty" yaml:"cascade,omitempty"`
	ReadOnly                 bool   `json:"read_only,omitempty" yaml:"read_only,omitempty"`
	CheckMetadataConsistency *bool  `json:"check_metadata_consistency,omitempty" yaml:"check_metadata_consistency,omitempty"`
}

type RunSQLResultType string

type PGRunSQLOutput struct {
	ResultType RunSQLResultType `json:"result_type" yaml:"result_type"`
	Result     [][]string       `json:"result" yaml:"result"`
}

type MSSQLRunSQLInput PGRunSQLInput
type MSSQLRunSQLOutput struct {
	ResultType RunSQLResultType `json:"result_type" yaml:"result_type"`
	Result     [][]interface{}  `json:"result" yaml:"result"`
}
