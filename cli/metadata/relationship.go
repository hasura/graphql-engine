package metadata

type relationship struct {
	RelType string `json:"rel_type"`

	RelName string `json:"rel_name"`

	RefTableSchema string `json:"ref_table_table_schema"`

	RefTableName string `json:"ref_table"`

	RelDef interface{} `json:"rel_def"`
}

func (r *relationship) Track(tableName, schemaName string) hasuraDBQuery {
	query := hasuraDBQuery{}
	switch r.RelType {
	case "object":
		query.QueryType = createObjectRelationShip
		query.Args = objectRelArg{
			Table: tableArg{
				Name:   tableName,
				Schema: schemaName,
			},
			Name:  r.RelName,
			Using: r.RelDef,
		}
	case "array":
		query.QueryType = createArrayRelationShip
		query.Args = arrayRelArg{
			Name: r.RelName,
			Table: tableArg{
				Name:   tableName,
				Schema: schemaName,
			},
			Using: r.RelDef,
		}
	}
	return query
}

func (r *relationship) UnTrack(tableName, schemaName string) hasuraDBQuery {
	return hasuraDBQuery{
		QueryType: dropRelationShip,
		Args: dropRelationShipArg{
			Table: tableArg{
				Name:   tableName,
				Schema: schemaName,
			},
			Relationship: r.RelName,
		},
	}
}
