package metadata

const (
	trackTable               string = "track_table"
	unTrackTable                    = "untrack_table"
	createObjectRelationShip        = "create_object_relationship"
	createArrayRelationShip         = "create_array_relationship"
	dropRelationShip                = "drop_relationship"
)

type hasuraDBQuery struct {
	QueryType string      `json:"type"`
	Args      interface{} `json:"args"`
}

type hasuraDBulkQuery struct {
	QueryType string          `json:"type"`
	Args      []hasuraDBQuery `json:"args"`
}

type sqlQuery struct {
	SQL string `json:"sql"`
}

type sqlRes struct {
	ResultType string `json:"result_type"`

	Result [][]string `json:"result"`
}

type tableArg struct {
	Schema string `json:"schema"`
	Name   string `json:"name"`
}

type unTrackTableArg struct {
	Table   tableArg `json:"table"`
	Cascade bool     `json:"cascade,omitempty"`
}

type objectRelArg struct {
	Table tableArg    `json:"table"`
	Name  string      `json:"name"`
	Using objRelUsing `json:"using"`
}

type arrayRelArg struct {
	Table tableArg     `json:"table"`
	Name  string       `json:"name"`
	Using arraRelUsing `json:"using"`
}

type dropRelationShipArg struct {
	Table        tableArg `json:"table"`
	Relationship string   `json:"relationship"`
}

type objRelUsing struct {
	ForeignKeyConstraintOn string               `json:"foreign_key_constraint_on,omitempty"`
	ManualConfiguration    *manualConfiguration `json:"manual_configuration,omitempty"`
}

type arraRelUsing struct {
	ForeignKeyConstraintOn *arrayForeignKeyConstraintOn `json:"foreign_key_constraint_on,omitempty"`
	ManualConfiguration    *manualConfiguration         `json:"manual_configuration,omitempty"`
}

type arrayForeignKeyConstraintOn struct {
	Table  tableArg `json:"table"`
	Column string   `json:"column"`
}

type manualConfiguration struct {
	RemoteTable   tableArg          `json:"remote_table"`
	ColumnMapping map[string]string `json:"column_mapping"`
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
