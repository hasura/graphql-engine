package hasura

const (
	CommandOK RunSQLResultType = "CommandOk"
	TuplesOK  RunSQLResultType = "TuplesOk"
)

type RunSQLResultType string

type PGSourceOps interface {
	PGRunSQL(input PGRunSQLInput) (response *PGRunSQLOutput, err error)
}

type PGRunSQLInput struct {
	SQL                      string `json:"sql" yaml:"sql"`
	Source                   string `json:"source,omitempty" yaml:"source,omitempty"`
	Cascade                  bool   `json:"cascade,omitempty" yaml:"cascade,omitempty"`
	ReadOnly                 bool   `json:"read_only,omitempty" yaml:"read_only,omitempty"`
	CheckMetadataConsistency *bool  `json:"check_metadata_consistency,omitempty" yaml:"check_metadata_consistency,omitempty"`
}

type PGRunSQLOutput struct {
	ResultType RunSQLResultType `json:"result_type" yaml:"result_type"`
	Result     [][]string       `json:"result" yaml:"result"`
}

type MSSQLSourceOps interface {
	MSSQLRunSQL(input MSSQLRunSQLInput) (response *MSSQLRunSQLOutput, err error)
}
type MSSQLRunSQLInput PGRunSQLInput
type MSSQLRunSQLOutput struct {
	ResultType RunSQLResultType `json:"result_type" yaml:"result_type"`
	Result     [][]interface{}  `json:"result" yaml:"result"`
}

type CitusSourceOps interface {
	CitusRunSQL(input CitusRunSQLInput) (response *CitusRunSQLOutput, err error)
}

type CitusRunSQLInput PGRunSQLInput

type CitusRunSQLOutput PGRunSQLOutput

type BigQuerySourceOps interface {
	BigQueryRunSQL(input BigQueryRunSQLInput) (response *BigQueryRunSQLOutput, err error)
}

type BigQueryRunSQLInput PGRunSQLInput

type BigQueryRunSQLOutput PGRunSQLOutput

type CockroachSourceOps interface {
	CockroachRunSQL(input CockroachRunSQLInput) (response *CockroachRunSQLOutput, err error)
}

type CockroachRunSQLInput PGRunSQLInput

type CockroachRunSQLOutput PGRunSQLOutput
