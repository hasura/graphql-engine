package metadata

type v1Query struct {
	QueryType string      `json:"type" yaml:"type"`
	Args      interface{} `json:"args" yaml:"args"`
}

type v1BulkQuery struct {
	QueryType string    `json:"type" yaml:"type"`
	Args      []v1Query `json:"args" yaml:"argss"`
}

type tableArg struct {
	Schema string `json:"schema" yaml:"schema"`
	Name   string `json:"name" yaml:"name"`
}

type trackTableArg struct {
	Table tableArg `json:"table" yaml:"table"`
}

type unTrackTableArg struct {
	Table   tableArg `json:"table" yaml:"table"`
	Cascade bool     `json:"cascade,omitempty" yaml:"cascade,omitempty"`
}

type createObjectRelationship struct {
	Table tableArg    `json:"table" yaml:"table"`
	Name  string      `json:"name" yaml:"name"`
	Using interface{} `json:"using" yaml:"using"`
}

type createArrayRelationship struct {
	Table tableArg    `json:"table" yaml:"table"`
	Name  string      `json:"name" yaml:"name"`
	Using interface{} `json:"using" yaml:"using"`
}

type dropRelationship struct {
	Table        tableArg `json:"table" yaml:"table"`
	Relationship string   `json:"relationship" yaml:"relationship"`
}

type manualConfiguration struct {
	RemoteTable   interface{}       `json:"remote_table" yaml:"remote_table"`
	ColumnMapping map[string]string `json:"column_mapping" yaml:"column_mapping"`
}

type objectRelationshipUsing struct {
	ForeignKeyConstraintOn string               `json:"foreign_key_constraint_on,omitempty" yaml:"foreign_key_constraint_on,omitempty"`
	ManualConfiguration    *manualConfiguration `json:"manual_configuration,omitempty" yaml:"manual_configuration,omitempty"`
}

type foreignKeyConstraintOn struct {
	Table  interface{} `json:"table" yaml:"table"`
	Column string      `json:"column" yaml:"column"`
}

type arrayRelationshipUsing struct {
	ForeignKeyConstraintOn *foreignKeyConstraintOn `json:"foreign_key_constraint_on,omitempty" yaml:"foreign_key_constraint_on,omitempty"`
	ManualConfiguration    *manualConfiguration    `json:"manual_configuration,omitempty" yaml:"manual_configuration,omitempty"`
}
