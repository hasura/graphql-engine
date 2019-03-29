package metadata

const (
	trackTable               string = "track_table"
	unTrackTable                    = "untrack_table"
	createObjectRelationShip        = "create_object_relationship"
	createArrayRelationShip         = "create_array_relationship"
	dropRelationShip                = "drop_relationship"
)

type hasuraDBQuery struct {
	QueryType string      `json:"type" yaml:"type"`
	Args      interface{} `json:"args" yaml:"args"`
}

type hasuraDBulkQuery struct {
	QueryType string          `json:"type" yaml:"type"`
	Args      []hasuraDBQuery `json:"args" yaml:"argss"`
}

type sqlQuery struct {
	SQL string `json:"sql" yaml:"sql"`
}

type sqlRes struct {
	ResultType string `json:"result_type" yaml:"result_type"`

	Result [][]string `json:"result" yaml:"result"`
}

type tableArg struct {
	Schema string `json:"schema" yaml:"schema"`
	Name   string `json:"name" yaml:"name"`
}

type unTrackTableArg struct {
	Table   tableArg `json:"table" yaml:"table"`
	Cascade bool     `json:"cascade,omitempty" yaml:"cascade,omitempty"`
}

type objectRelArg struct {
	Table tableArg    `json:"table" yaml:"table"`
	Name  string      `json:"name" yaml:"name"`
	Using objRelUsing `json:"using" yaml:"using"`
}

type arrayRelArg struct {
	Table tableArg     `json:"table" yaml:"table"`
	Name  string       `json:"name" yaml:"name"`
	Using arraRelUsing `json:"using" yaml:"using"`
}

type dropRelationShipArg struct {
	Table        tableArg `json:"table" yaml:"table"`
	Relationship string   `json:"relationship" yaml:"relationship"`
}

type objRelUsing struct {
	ForeignKeyConstraintOn string               `json:"foreign_key_constraint_on,omitempty" yaml:"foreign_key_constraint_on,omitempty"`
	ManualConfiguration    *manualConfiguration `json:"manual_configuration,omitempty" yaml:"manual_configuration,omitempty"`
}

type arraRelUsing struct {
	ForeignKeyConstraintOn *arrayForeignKeyConstraintOn `json:"foreign_key_constraint_on,omitempty" yaml:"foreign_key_constraint_on,omitempty"`
	ManualConfiguration    *manualConfiguration         `json:"manual_configuration,omitempty" yaml:"manual_configuration,omitempty"`
}

type arrayForeignKeyConstraintOn struct {
	Table  tableArg `json:"table" yaml:"table"`
	Column string   `json:"column" yaml:"column"`
}

type manualConfiguration struct {
	RemoteTable   tableArg          `json:"remote_table" yaml:"remote_table"`
	ColumnMapping map[string]string `json:"column_mapping" yaml:"column_mapping"`
}

func newBulkQuery() *hasuraDBulkQuery {
	query := &hasuraDBulkQuery{
		QueryType: "bulk",
		Args:      make([]hasuraDBQuery, 0),
	}
	return query
}

func newSQLQuery(sql string) *hasuraDBQuery {
	query := &hasuraDBQuery{
		QueryType: "run_sql",
		Args: sqlQuery{
			SQL: sql,
		},
	}
	return query
}

func (h *hasuraDBulkQuery) appendQuery(query hasuraDBQuery) {
	h.Args = append(h.Args, query)
}
